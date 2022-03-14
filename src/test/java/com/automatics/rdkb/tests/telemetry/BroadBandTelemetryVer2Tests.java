/*
 * Copyright 2021 Comcast Cable Communications Management, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.automatics.rdkb.tests.telemetry;

import java.text.SimpleDateFormat;
import java.util.Date;

import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandPropertyKeyConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants;
import com.automatics.rdkb.constants.WebPaParamConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadBandRfcFeatureControlUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.cdl.BroadBandCodeDownloadUtils;
import com.automatics.rdkb.utils.cdl.FirmwareDownloadUtils;
import com.automatics.rdkb.utils.telemetry.BroadBandTelemetry2Utils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandTelemetryVer2Tests extends AutomaticsTestBase {

    private boolean isTelemetryVerOneEnabled = false;
    private boolean isTelemetryVerTwoProcessUp = false;
    private String stepNum = "S1";

    @BeforeMethod
    public void executePreConditionForTelemetryVerTwo(Object[] params) {
	LOGGER.debug(" STARTING BEFORE METHOD FOR TELEMETRY 2.0 ");
	try {

	    if (null != params[0] && (params[0] instanceof Dut)) {
		Dut device = (Dut) params[0];
		boolean status = false;
		String response = null;
		long boxUpTimeInSeconds = 0;
		long boxUpTimeInSecondsAfterReboot = 0;
		boolean isSTBAccessible = false;

		/**
		 * Executing pre-condition for telemetry 2.0
		 */
		LOGGER.info("################### STARTING PRE-CONFIGURATIONS FOR TELEMETRY 2.0 ###################");
		LOGGER.info("PRE-CONDITION STEPS");

		// check the t2 version from stb.properties and update the enum

		String t2version = BroadBandTelemetry2Utils.validateT2Version(device, tapEnv);
		LOGGER.info("T2 version from stb.properties: " + t2version);
		BroadBandTestConstants.TELEMETRY_2_WEBPA_SETTINGS.VERSION.setDefaultT2Config(t2version);

		LOGGER.info("##################################################################################");
		LOGGER.info("PRE-CONDITION 1"
			+ " :DESCRIPTION : Validate if device is on telemetry 2.0 settings. Else, set telemetry 2.0 settings on gateway via RFC");
		LOGGER.info("PRE-CONDITION 1"
			+ " :ACTION : Execute the command ps|grep -i telemetry and check if telemetry 2.0 process is up and running. Else set telemetry 2.0 settings ");
		LOGGER.info("PRE-CONDITION 1"
			+ " : EXPECTED : The device should be successfully upgraded to telemetry 2.0 settings ");
		LOGGER.info("##################################################################################");

		status = BroadBandCommonUtils.verifyProcessRunningStatus(device, tapEnv, false,
			BroadBandTestConstants.PROCESS_NAME_TELEMETRY_2_0);
		if (!status) {
		    response = tapEnv.executeWebPaCommand(device,
			    BroadBandWebPaConstants.WEBPA_PARAM_FOR_TELEMETRY_2_0_VERSION);
		    if (CommonMethods.isNotNull(response)) {
			isTelemetryVerOneEnabled = response.equals(BroadBandTestConstants.STRING_VALUE_ONE);
		    }
		    if (BroadBandRfcFeatureControlUtils.executePreconditionForRfcTests(device, tapEnv,
			    AutomaticsTapApi.getSTBPropsValue(BroadBandTestConstants.PROP_KEY_TELEMETRY_VER_2_CONFIG)
				    .replaceAll(BroadBandTestConstants.TELEMETRY_ENABLE_VAUE,
					    BroadBandTestConstants.TELEMETRY_2_WEBPA_SETTINGS.FEATURE_ENABLE
						    .getDefaultT2Config())
				    .replaceAll(BroadBandTestConstants.TELEMETRY_VERSION_VALUE,
					    BroadBandTestConstants.TELEMETRY_2_WEBPA_SETTINGS.VERSION
						    .getDefaultT2Config())
				    .replaceAll(BroadBandTestConstants.TELEMETRY_CONFIG_URL_VALUE,
					    BroadBandTestConstants.TELEMETRY_2_WEBPA_SETTINGS.CONFIG_URL
						    .getDefaultT2Config()))) {
			status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_RFC_CONTROL,
				BroadBandTestConstants.STRING_CONSTANT_1,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
				BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
			if (!status) {
			    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
				    BroadBandWebPaConstants.WEBPA_PARAM_RFC_CONTROL, BroadBandTestConstants.CONSTANT_2,
				    BroadBandTestConstants.STRING_CONSTANT_1,
				    BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
				    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			}
			if (CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv)) {
			    status = BroadBandRfcFeatureControlUtils
				    .verifyParameterUpdatedByRfc(device, tapEnv,
					    BroadBandTestConstants.TELEMETRY_2_WEBPA_SETTINGS.FEATURE_ENABLE.getParam(),
					    BroadBandTestConstants.TELEMETRY_2_WEBPA_SETTINGS.FEATURE_ENABLE
						    .getDefaultT2Config())
				    .isStatus()
				    && BroadBandRfcFeatureControlUtils.verifyParameterUpdatedByRfc(device, tapEnv,
					    BroadBandTestConstants.TELEMETRY_2_WEBPA_SETTINGS.CONFIG_URL.getParam(),
					    BroadBandTestConstants.TELEMETRY_2_WEBPA_SETTINGS.CONFIG_URL
						    .getDefaultT2Config())
					    .isStatus()
				    && BroadBandRfcFeatureControlUtils.verifyParameterUpdatedByRfc(device, tapEnv,
					    BroadBandTestConstants.TELEMETRY_2_WEBPA_SETTINGS.VERSION.getParam(),
					    BroadBandTestConstants.TELEMETRY_2_WEBPA_SETTINGS.VERSION
						    .getDefaultT2Config())
					    .isStatus()
				    && BroadBandTelemetry2Utils.verifyTelemetry2ConfigurationViaWebpa(device, tapEnv);
			    boxUpTimeInSecondsAfterReboot = (CommonUtils.getBoxUptimeInSeconds(device, tapEnv) * 1000)
				    + BroadBandTestConstants.EIGHT_MINUTE_IN_MILLIS;
			    do {
				isSTBAccessible = CommonMethods.isSTBAccessible(device);
				if (!isSTBAccessible) {
				    LOGGER.error(
					    "Device is not accessible while checking for telemetry process is up and running");
				}
				boxUpTimeInSeconds = CommonUtils.getBoxUptimeInSeconds(device, tapEnv);
				status = BroadBandCommonUtils.verifyProcessRunningStatus(device, tapEnv, false,
					BroadBandTestConstants.PROCESS_NAME_TELEMETRY_2_0);
			    } while (!status && (boxUpTimeInSeconds * 1000 < boxUpTimeInSecondsAfterReboot)
				    && isSTBAccessible && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
					    BroadBandTestConstants.FIFTEEN_SECONDS_IN_MILLIS));

			}
		    }
		}
		isTelemetryVerTwoProcessUp = status;
	    } else {
		throw new TestException(RDKBTestConstants.PRE_CONDITION_ERROR
			+ "PRE_CONDITION_FAILED :Telemetry 2 settings hasn't been configured successfully on the gateway ");
	    }

	} catch (Exception e) {
	    LOGGER.error("Exception occured while executing pre-condition for telemetry 2.0..", e);
	}
	LOGGER.debug(" ENDING BEFORE METHOD FOR TELEMETRY 2.0 ");
    }

    /**
     * Verify T2 component log files, process and other basic checks
     * <ol>
     * *
     * <li>Pre-condition : Factory reset device using webpa</li>
     * <li>1. Validate Factory default values of telemetry 2 config parameters</li>
     * <li>2. Validate if telemetry 2 params can be set/get via webpa</li>
     * <li>3. Validate if telemetry 2 params can be set/get via dmcli</li>
     * </ol>
     * 
     * @param device
     *            {@link Instanceof Dut}
     *
     * @author Sathurya_R
     * @refactor yamini.s
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
    @TestDetails(testUID = "TC-RDKB-TELEMETRY-VER2-1001")
    public void testToVerifyTelemetryVersion2DefaultValues(Dut device) {
	// Variable Declaration begins
	String testCaseId = "TC-RDKB-TELEMETRY-VER2-101";
	String errorMessage = null;
	int stepNumber = 1;
	boolean status = false;
	String response = null;
	boolean isFactoryReset = false;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-TELEMETRY-VER2-1001");
	LOGGER.info("TEST DESCRIPTION: Verify T2 component log files, process and other basic checks");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("Pre-condition : Factory reset device using webpa ");
	LOGGER.info("1. Validate Factory default values of telemetry 2 config parameters ");
	LOGGER.info("2. Validate if telemetry 2 params can be set/get via webpa ");
	LOGGER.info("3. Validate if telemetry 2 params can be set/get via dmcli ");
	LOGGER.info("#######################################################################################");

	try {

	    if (!isTelemetryVerTwoProcessUp) {
		{
		    throw new TestException(RDKBTestConstants.PRE_CONDITION_ERROR
			    + "PRE_CONDITION_FAILED :Telemetry 2 process is not running on the gateway "
			    + device.getHostMacAddress());
		}
	    }

	    /**
	     * Executing pre-condition to perform factory reset
	     */
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    LOGGER.info("##################################################################################");
	    LOGGER.info("PRE-CONDITION 1" + " :DESCRIPTION : Factory reset device using webpa");
	    LOGGER.info("PRE-CONDITION 1"
		    + " :ACTION : Execute Param :Device.X_CISCO_COM_DeviceControl.FactoryReset Value : \"Router,Wifi,VoIP,Dect,MoCA\" ");
	    LOGGER.info("PRE-CONDITION 1" + " : EXPECTED : Device should go factory reset successfully ");
	    LOGGER.info("##################################################################################");

	    status = BroadBandCommonUtils.performFactoryResetAndWaitForWebPaProcessToUp(tapEnv, device);
	    isFactoryReset = status;
	    if (status) {
		LOGGER.info("PRE-CONDITION 1: Successfully performed factory reset via webpa");
	    } else {
		throw new TestException(RDKBTestConstants.PRE_CONDITION_ERROR
			+ "PRE_CONDITION_FAILED :Failed to perform factory reset via webpa "
			+ device.getHostMacAddress());
	    }
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
	    LOGGER.info("##################################################################################");

	    String stepNum = "S" + stepNumber;
	    status = false;
	    errorMessage = "Telemetry 2 params are not in appropriate factory defaults";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + " :DESCRIPTION : Validate Factory default values of telemetry 2 config parameters ");
	    LOGGER.info("STEP " + stepNumber + " :ACTION : Execute webpa get on the parameters ");
	    LOGGER.info("STEP " + stepNumber
		    + " : EXPECTED : The parameters should be in factory defaults/should be updated by RFC");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandTelemetry2Utils.validateFactoryDefaultsForTelemetryParams(device, tapEnv);
	    if (!status) {
		response = tapEnv.executeCommandUsingSsh(device, BroadBandTestConstants.CMD_CAT_DCMRFC_LOG);
		status = CommonMethods.isNotNull(response)
			&& CommonMethods.patternMatcher(response,
				".*" + BroadBandTestConstants.TELEMETRY_2_WEBPA_SETTINGS.FEATURE_ENABLE.getParam()
					+ ".*")
			&& CommonMethods.patternMatcher(response,
				".*" + BroadBandTestConstants.TELEMETRY_2_WEBPA_SETTINGS.VERSION.getParam() + ".*")
			&& CommonMethods.patternMatcher(response,
				".*" + BroadBandTestConstants.TELEMETRY_2_WEBPA_SETTINGS.CONFIG_URL.getParam() + ".*");
	    }

	    if (status) {
		LOGGER.info("STEP " + stepNum + ": ACTUAL : Telemetry 2 params are in appropriate factory defaults");
	    } else {
		LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    ++stepNumber;
	    stepNum = "S" + stepNumber;
	    status = false;
	    errorMessage = "Telemetry 2 params are not working as expected via webpa";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP " + stepNumber + " :DESCRIPTION : Validate if telemetry 2 params can be set/get via webpa ");
	    LOGGER.info("STEP " + stepNumber + " :ACTION : Execute webpa set and get on the parameters ");
	    LOGGER.info("STEP " + stepNumber + " : EXPECTED : The parameters set and get should work as expected");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandTelemetry2Utils.validateIfTelemetryParamsCanBeSetViaWebpaOrDmcli(device, false, tapEnv);

	    if (status) {
		LOGGER.info("STEP " + stepNum + ": ACTUAL : Telemetry 2 params are working as expected via webpa");
	    } else {
		LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    ++stepNumber;
	    stepNum = "S" + stepNumber;
	    status = false;
	    errorMessage = "Telemetry 2 params are not working as expected via dmcli";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP " + stepNumber + " :DESCRIPTION : Validate if telemetry 2 params can be set/get via dmcli ");
	    LOGGER.info("STEP " + stepNumber + " :ACTION : Execute dmcli set and get on the parameters ");
	    LOGGER.info("STEP " + stepNumber + " : EXPECTED : The parameters set and get should work as expected");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandTelemetry2Utils.validateIfTelemetryParamsCanBeSetViaWebpaOrDmcli(device, true, tapEnv);

	    if (status) {
		LOGGER.info("STEP " + stepNum + ": ACTUAL : Telemetry 2 params are working as expected via dmcli");
	    } else {
		LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	} finally {

	    if (isFactoryReset) {
		BroadBandPostConditionUtils.executePostConditionToReActivateDevice(device, tapEnv, false,
			BroadBandTestConstants.CONSTANT_1);
	    }
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-TELEMETRY-VER2-1001");

    }

    /**
     * Verify T2 component log files, process and other basic checks
     * <ol>
     * <li>Pre-condition : Validate if the T2 settings are already present else set the settings and reboot the
     * device</li>
     * <li>Verify if the 'telemetry2_0' process is up after enabling telemetry 2.0</li>
     * <li>Verify if the log file /rdklogs/logs/telemetry_2.txt.0 is present in Arm console</li>
     * <li>Verify if 'dcmscript.log' indicates T2 is enabled</li>
     * <li>Verify if T2 markers are getting uploaded to splunk</li>
     * <li>Verify if T2 logs are present in splunk</li>
     * <li>Validate if the splunk logs mandatory parameters</li>
     * <li>Verify from the splunk that telemetry upload is successful and in appropriate format indicating that it is
     * from T2 component</li>
     * <li>Verify if telemetry kick starts telemetry 2 process if killed</li>
     * <li>Verify if the 'telemetry2_0' process is up after enabling telemetry 2.0</li>
     * <li>Verify if the log file /rdklogs/logs/telemetry_2.txt.0 is present in Arm console</li>
     * <li>Verify if 'dcmscript.log' indicates T2 is enabled</li>
     * <li>Verify if T2 markers are getting uploaded to splunk</li>
     * <li>Verify if T2 logs are present in splunk</li>
     * <li>Validate if the splunk logs mandatory parameters</li>
     * <li>Verify from the splunk that telemetry upload is successful and in appropriate format indicating that it is
     * from T2 component</li>
     * </ol>
     * 
     * @param device
     *            {@link Instanceof Dut}
     * 
     * @author Sathurya_R
     * @refactor yamini.s
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
    @TestDetails(testUID = "TC-RDKB-TELEMETRY-VER2-1002")
    public void testToVerifyTelemetryVersiontTwoBasics(Dut device) {
	// Variable Declaration begins
	String testCaseId = "TC-RDKB-TELEMETRY-VER2-102";
	String errorMessage = null;
	int stepNumber = 1;
	boolean status = false;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-TELEMETRY-VER2-1002");
	LOGGER.info("TEST DESCRIPTION: Verify T2 component log files, process and other basic checks");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info(
		"Pre-condition : Validate if the T2 process is up on the device else set the settings and reboot the device ");
	LOGGER.info("1. Verify if the 'telemetry2_0' process is up after enabling telemetry 2.0 ");
	LOGGER.info("2. Verify if the log file /rdklogs/logs/telemetry_2.txt.0 is present in Arm console ");
	LOGGER.info("3. Verify if 'dcmscript.log' indicates T2 is enabled ");
	LOGGER.info("4. Verify if T2 markers are getting uploaded to splunk ");
	LOGGER.info("5. Verify if T2 logs are present in splunk ");
	LOGGER.info("6. Validate if the splunk logs mandatory parameters ");
	LOGGER.info("7. Verify from the splunk that  telemetry upload is successful "
		+ "and in appropriate format indicating that it is from T2 component ");
	LOGGER.info("8. Verify if telemetry kick starts telemetry 2 process if killed ");
	LOGGER.info("9. Verify if the 'telemetry2_0' process is up after enabling telemetry 2.0 ");
	LOGGER.info("10. Verify if the log file /rdklogs/logs/telemetry_2.txt.0 is present in Arm console ");
	LOGGER.info("11. Verify if 'dcmscript.log' indicates T2 is enabled ");
	LOGGER.info("12. Verify if T2 markers are getting uploaded to splunk ");
	LOGGER.info("13. Verify if T2 logs are present in splunk ");
	LOGGER.info("14. Validate if the splunk logs mandatory parameters ");
	LOGGER.info("15. Verify from the splunk that  telemetry upload is successful and "
		+ "in appropriate format indicating that it is from T2 component ");
	LOGGER.info("#######################################################################################");

	try {

	    if (!isTelemetryVerTwoProcessUp) {
		{
		    throw new TestException(RDKBTestConstants.PRE_CONDITION_ERROR
			    + "PRE_CONDITION_FAILED :Telemetry 2 process is not running on the gateway "
			    + device.getHostMacAddress());
		}
	    }

	    /**
	     * Steps 1 to 7
	     */
	    executeCommonStepsForTelemetry2(device, stepNumber, testCaseId, tapEnv);

	    stepNumber = 8;
	    stepNum = "S" + stepNumber;
	    status = false;
	    errorMessage = "Telemetry 2 process is not getting retstarted by SelfHeal";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + " :DESCRIPTION : Verify if telemetry kick starts telemetry 2 process if killed ");
	    LOGGER.info("STEP " + stepNumber
		    + " :ACTION : Kill the telemetry 2 process and verify from self heal logs if it gets restarted ");
	    LOGGER.info("STEP " + stepNumber + " : EXPECTED : The self heal should restart T2 agent if killed");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandTelemetry2Utils.verifySelfHealKicksStartsTelemetry2Process(device, tapEnv);

	    if (status) {
		LOGGER.info("STEP " + stepNum
			+ ": ACTUAL : Telemetry 2 process has been restarted successfully after being killed");
	    } else {
		LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * Steps 9 to 13
	     */
	    executeCommonStepsForTelemetry2(device, ++stepNumber, testCaseId, tapEnv);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-TELEMETRY-VER2-1002");

    }

    /**
     * Verify negative scenarios for Telemetry version 2
     * <ol>
     * <li>Pre-condition : VALIDATE IF THE T2 SETTINGS ARE ALREADY PRESENT ELSE SET THE SETTINGS AND REBOOT THE
     * DEVICE</li>
     * <li>Verify changing Telemetry.ConfigURL to blank value and reboot</li>
     * <li>Verify changing Telemetry.ConfigURL to wrong value and reboot</li>
     * <li>Post-condition : SET THE DEVICE BACK TO TELEMETRY VERSION 2 SETTINGS</li>
     * </ol>
     * 
     * @param device
     *            {@link Dut}
     * 
     * @author Sathurya_R
     * @refactor Said.h
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
    @TestDetails(testUID = "TC-RDKB-TELEMETRY-VER2-1004")
    public void testToVerifyTelemetryVersion2NegativeScenario(Dut device) {
	// Variable Declaration begins
	String testCaseId = "TC-RDKB-TELEMETRY-VER2-104";
	String errorMessage = null;
	int stepNumber = 1;
	boolean status = false;
	String response = null;
	String telemetryLogs = null;
	// Variable Declaration Ends

	try {

	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-TELEMETRY-VER2-1004");
	    LOGGER.info("TEST DESCRIPTION: Verify negative scenarios for Telemetry version 2");

	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info(
		    "Pre-condition : VALIDATE IF THE T2 SETTINGS ARE ALREADY PRESENT ELSE SET THE SETTINGS AND REBOOT THE DEVICE ");
	    LOGGER.info("1. Verify changing Telemetry.ConfigURL to blank value and reboot");
	    LOGGER.info("2. Verify changing Telemetry.ConfigURL to wrong value and reboot");
	    LOGGER.info("3. Verify re-try is happening for wrong value of Telemetry.ConfigURL ");
	    LOGGER.info("4. Verify if re-try is happening within 180s");
	    LOGGER.info("Post-condition : SET THE DEVICE BACK TO TELEMETRY VERSION 2 SETTINGS  ");
	    LOGGER.info("#######################################################################################");

	    if (!isTelemetryVerTwoProcessUp) {
		{
		    throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
			    + "PRE_CONDITION_FAILED :Telemetry 2 process is not running on the gateway "
			    + device.getHostMacAddress());
		}
	    }

	    stepNum = "S" + stepNumber;
	    status = false;
	    errorMessage = "Expected logs are not seen in telemetry2_0.txt.0";
	    long startTime = 0;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + " :DESCRIPTION : Verify changing Telemetry.ConfigURL to blank value");
	    LOGGER.info("STEP " + stepNumber + " :ACTION : Set the telemetry URL to be blank to the parameter "
		    + "Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.Telemetry.ConfigURL and reboot the device ");
	    LOGGER.info("STEP " + stepNumber
		    + " : EXPECTED : The log stating that 'URL doesn't start with https or is invalid' "
		    + "should be there in telemetry 2 logs");
	    LOGGER.info("**********************************************************************************");
	    try {
		tapEnv.executeCommandUsingSsh(device,
			BroadBandCommandConstants.TAIL_LOG_TO_BACKUP_FILE.replaceAll(
				BroadBandTestConstants.STRING_REPLACE,
				BroadBandTestConstants.FILE_PATH_TELEMETRY_2_0.replace(
					BroadBandCommandConstants.DIRECTORY_LOGS,
					BroadBandTestConstants.EMPTY_STRING)));
		telemetryLogs = BroadBandCommandConstants.NAVIGATE_GIVEN_FILE_IN_NVRAM.replace(
			BroadBandTestConstants.STRING_REPLACE,
			CommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.STRING_AUTOMATION_BACK_UP,
				BroadBandTestConstants.FILE_PATH_TELEMETRY_2_0.replace(
					BroadBandCommandConstants.DIRECTORY_LOGS,
					BroadBandTestConstants.EMPTY_STRING)));
	    } catch (Exception e) {
		LOGGER.error("Execution occured during automation backup for telemetry file" + e.getMessage());
	    }
	    LOGGER.info("telemetryLogs- " + telemetryLogs);
	    if (BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandTestConstants.TELEMETRY_2_WEBPA_SETTINGS.CONFIG_URL.getParam(),
		    WebPaParamConstants.WebPaDataTypes.STRING.getValue(), " ")
		    && CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv)) {
		startTime = System.currentTimeMillis();
		do {
		    response = tapEnv.executeCommandUsingSsh(device,
			    BroadBandTestConstants.CMD_GET_TELEMETRY_VER_TWO_LOGS);
		    status = CommonMethods.isNotNull(response) && CommonMethods.patternMatcher(response,
			    ".*" + BroadBandTestConstants.LOGS_INDICATE_INVALID_TELEMETRY2_URL + ".*");
		    if (!status) {
			response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_CAT
				+ BroadBandTestConstants.SINGLE_SPACE_CHARACTER + telemetryLogs);
			status = CommonMethods.isNotNull(response) && CommonMethods.patternMatcher(response,
				".*" + BroadBandTestConstants.LOGS_INDICATE_INVALID_TELEMETRY2_URL + ".*");
		    }

		} while (!status
			&& (System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS
			&& BroadBandCommonUtils.hasWaitForDuration(tapEnv,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    }

	    if (status) {
		LOGGER.info("STEP " + stepNum
			+ ": ACTUAL : The log 'URL doesn't start with https or is invalid' is present in telemetry2_0.txt.0 ");
	    } else {
		LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    ++stepNumber;
	    stepNum = "S" + stepNumber;
	    status = false;
	    errorMessage = "Expected logs are not seen in telemetry2_0.txt.0";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + " :DESCRIPTION : Verify changing Telemetry.ConfigURL to wrong value");
	    LOGGER.info("STEP " + stepNumber + " :ACTION : Set the telemetry URL to be blank to the parameter "
		    + "Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.Telemetry.ConfigURL and reboot the device ");
	    LOGGER.info("STEP " + stepNumber + " : EXPECTED : The log stating that 'T2:Curl GET of XCONF data failed' "
		    + "should be there in telemetry 2 logs");
	    LOGGER.info("**********************************************************************************");

	    if (BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandTestConstants.TELEMETRY_2_WEBPA_SETTINGS.CONFIG_URL.getParam(),
		    WebPaParamConstants.WebPaDataTypes.STRING.getValue(),
		    BroadBandTestConstants.TELEMETRY_2_WEBPA_SETTINGS.CONFIG_URL.getDefaultT2Config().substring(0, 10))
		    && CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv)) {
		startTime = System.currentTimeMillis();
		do {
		    response = tapEnv.executeCommandUsingSsh(device,
			    BroadBandTestConstants.CMD_GET_TELEMETRY_VER_TWO_LOGS);
		    status = CommonMethods.isNotNull(response) && CommonMethods.patternMatcher(response,
			    ".*" + BroadBandTestConstants.LOGS_INDICATE_XCONF_GET_INVALID + ".*");
		    if (!status) {
			response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_CAT
				+ BroadBandTestConstants.SINGLE_SPACE_CHARACTER + telemetryLogs);
			status = CommonMethods.isNotNull(response) && CommonMethods.patternMatcher(response,
				".*" + BroadBandTestConstants.LOGS_INDICATE_XCONF_GET_INVALID + ".*");
		    }

		} while (!status
			&& (System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS
			&& BroadBandCommonUtils.hasWaitForDuration(tapEnv,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    }

	    if (status) {
		LOGGER.info("STEP " + stepNum
			+ ": ACTUAL : The log 'T2:Curl GET of XCONF data failed' is present in telemetry2_0.txt.0 ");
	    } else {
		LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    ++stepNumber;
	    stepNum = "S" + stepNumber;
	    status = false;
	    errorMessage = "Expected re-try logs are not seen in telemetry2_0.txt.0";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + " :DESCRIPTION : Verify 3 re-try are happening as Telemetry.ConfigURL has wrong value");
	    LOGGER.info("STEP " + stepNumber
		    + " :ACTION : Search for 'Waiting for 180 sec before trying fetchRemoteConfiguration, No.of tries : 3' in telemetry2_0.txt.0");
	    LOGGER.info("STEP " + stepNumber
		    + " : EXPECTED : The log stating re-try msg 'Waiting for 180 sec before trying fetchRemoteConfiguration, No.of tries : 3' "
		    + "should be there in telemetry 2 logs");
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("Verifying re-try logs");
	    startTime = System.currentTimeMillis();
	    do {
		response = tapEnv.executeCommandUsingSsh(device, BroadBandTestConstants.CMD_GET_TELEMETRY_VER_TWO_LOGS);
		status = CommonMethods.isNotNull(response) && CommonMethods.patternMatcher(response,
			BroadBandTestConstants.LOGS_INDICATE_XCONF_RE_TRY_MSG);
		if (!status) {
		    response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_CAT
			    + BroadBandTestConstants.SINGLE_SPACE_CHARACTER + telemetryLogs);
		    status = CommonMethods.isNotNull(response) && CommonMethods.patternMatcher(response,
			    BroadBandTestConstants.LOGS_INDICATE_XCONF_RE_TRY_MSG);
		}
	    } while (!status && (System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    if (status) {
		LOGGER.info("STEP " + stepNum
			+ ": ACTUAL : The log 'Waiting for 180 sec before trying fetchRemoteConfiguration, No.of tries : 3' is present in telemetry2_0.txt.0 ");
	    } else {
		LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * Step 4: Verify re-try is happening in 180Seconds
	     */
	    helperMethodToCaptureTimeStamp(device, tapEnv, testCaseId, BroadBandTestConstants.CONSTANT_4,
		    telemetryLogs);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	} finally {
	    BroadBandPostConditionUtils.executePostConditionToRemoveBackUpFile(device, tapEnv,
		    BroadBandTestConstants.CONSTANT_1, telemetryLogs);
	}
    }

    /**
     * Method to CaptureTimeStamp
     * 
     * @param device
     *            {@link Dut}
     * @param tapEnv
     *            AutomaticsTapApi instance
     * @param testCaseId
     *            Test case id
     * @param stepNumber
     *            step number
     * @param telemetryLogs
     *            automation backup file path
     * 
     */
    public void helperMethodToCaptureTimeStamp(Dut device, AutomaticsTapApi tapEnv, String testCaseId,
	    Integer stepNumber, String telemetryLogs) {

	String stepNum = "S" + stepNumber;
	// long value to store time difference
	long timeDifference = BroadBandTestConstants.CONSTANT_0;
	// console log time from server 1 response
	Date secondserverhitTime = null;
	Date firstserverhitTime = null;
	String response = null;
	Boolean status = false;
	SimpleDateFormat format = new SimpleDateFormat(BroadBandTestConstants.TIME_FORMAT);
	String errorMessage = "Expected re-try time stamp is not 180secs";
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP " + stepNumber + " :DESCRIPTION : Verify re-try is happening in 180Seconds");
	LOGGER.info("STEP " + stepNumber
		+ " :ACTION : Execute grep -i 'Waiting for 180 sec before trying fetchRemoteConfiguration, No.of tries : 2' /rdklogs/logs/telemetry2_0.txt.0  ");
	LOGGER.info("STEP " + stepNumber
		+ " : EXPECTED : In the response should contain time stamp within 180s of difference between re-try");
	LOGGER.info("**********************************************************************************");
	try {
	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTestConstants.LOGS_INDICATE_XCONF_RE_TRY_MSG_2,
		    BroadBandTestConstants.FILE_PATH_TELEMETRY_2_0, BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

	    if (CommonMethods.isNull(response)) {
		response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTestConstants.LOGS_INDICATE_XCONF_RE_TRY_MSG_2, telemetryLogs,
			BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

	    }
	    LOGGER.info("Server 1 st hit Response " + response);
	    if (CommonMethods.isNotNull(response)) {
		// get the time stamp from the response output
		response = CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_TIME_FORMAT);
		if (CommonMethods.isNotNull(response)) {
		    // convert the time stamp to formated output
		    try {
			firstserverhitTime = format.parse(response);
		    } catch (Exception e) {
			LOGGER.error("Exception Occured validating 1st server hit time " + e.getMessage());
		    }
		}
	    }
	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTestConstants.LOGS_INDICATE_XCONF_RE_TRY_MSG,
		    BroadBandTestConstants.FILE_PATH_TELEMETRY_2_0, BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    if (CommonMethods.isNull(response)) {
		response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTestConstants.LOGS_INDICATE_XCONF_RE_TRY_MSG, telemetryLogs,
			BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    }
	    LOGGER.info("Server 2nd hit response " + response);
	    if (CommonMethods.isNotNull(response)) {
		// get the time stamp from the response output
		response = CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_TIME_FORMAT);
		if (CommonMethods.isNotNull(response)) {
		    // convert the time stamp to formated output
		    try {
			secondserverhitTime = format.parse(response);
		    } catch (Exception e) {
			LOGGER.error("Exception Occured validating 2nd server hit time" + e.getMessage());
		    }
		}
	    }
	    // calculate the difference in time
	    if (firstserverhitTime != null && secondserverhitTime != null) {
		timeDifference = secondserverhitTime.getTime() - firstserverhitTime.getTime();
	    }
	    // status true if time is >= 180000 miliseconds
	    status = (timeDifference >= 180000);
	    LOGGER.info("Time difference for fallback from primary server to secondary server :  " + timeDifference
		    + "and is :" + status);
	} catch (Exception e) {
	    LOGGER.error("Exception Occured validating fallback time stamp" + e.getMessage());
	}
	if (status) {
	    LOGGER.info("STEP :" + stepNumber + " ACTUAL : Successfully verified that the fallback happens within 5s");
	} else {
	    LOGGER.error("STEP :" + stepNumber + " ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

    }

    /**
     * Verify T2 component log files, process and other basic checks
     * <ol>
     * <li>Pre-condition : VALIDATE IF THE T2 SETTINGS ARE ALREADY PRESENT ELSE SET THE SETTINGS AND REBOOT THE
     * DEVICE</li>
     * <li>Verify if the 'telemetry2_0' process is up after enabling telemetry 2.0</li>
     * <li>Verify if the log file /rdklogs/logs/telemetry_2.txt.0 is present in Arm console</li>
     * <li>Verify if 'dcmscript.log' indicates T2 is enabled</li>
     * <li>Verify if T2 markers are getting uploaded to splunk</li>
     * <li>Verify if T2 logs are present in splunk</li>
     * <li>Validate if the splunk logs mandatory parameters</li>
     * <li>Verify from the splunk that telemetry upload is successful and in appropriate format indicating that it is
     * from T2 component</li>
     * <li>Validate if INFO and ERROR messages are present in /rdklogs/logs/telemetry2_0.txt.0</li>
     * <li>Validate if dca_utility.sh is not listed under cron jobs</li>
     * <li>Disable the telemetry 2.0 via webpa and reboot</li>
     * <li>Verify if the 'telemetry2_0' process is up after enabling telemetry 2.0</li>
     * <li>Validate if dca_utility.sh is listed under cron jobs</li>
     * <li>Validate the presence of logs in dcmscript.log</li>
     * <li>Verify log upload in splunk by searching using estb mac</li>
     * <li>Post-condition : SET THE DEVICE BACK TO TELEMETRY VERSION 2 SETTINGS</li>
     * </ol>
     * 
     * @param device
     *            {@link Instanceof Dut}
     * 
     * @author Sathurya_R
     * @refactor said.h
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
    @TestDetails(testUID = "TC-RDKB-TELEMETRY-VER2-1005")
    public void testToVerifyTelemetryVersionTwoFallBackScenario(Dut device) {
	// Variable Declaration begins
	String testCaseId = "TC-RDKB-TELEMETRY-VER2-105";
	String errorMessage = null;
	int stepNumber = 1;
	boolean status = false;
	// Variable Declaration Ends

	try {

	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-TELEMETRY-VER2-1005");
	    LOGGER.info("TEST DESCRIPTION: Verify fall back to T1 from T2 works as expected");

	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info(
		    "Pre-condition : VALIDATE IF THE T2 SETTINGS ARE ALREADY PRESENT ELSE SET THE SETTINGS AND REBOOT THE DEVICE ");
	    LOGGER.info("1. Verify if the 'telemetry2_0' process is up after enabling telemetry 2.0");
	    LOGGER.info("2. Verify if the log file /rdklogs/logs/telemetry_2.txt.0 is present in Arm console");
	    LOGGER.info("3. Verify if 'dcmscript.log' indicates T2 is enabled");
	    LOGGER.info("4. Verify if T2 markers are getting uploaded to splunk");
	    LOGGER.info("5. Verify if T2 logs are present in splunk");
	    LOGGER.info("6. Validate if the splunk logs mandatory parameters");
	    LOGGER.info(
		    "7. Verify from the splunk that telemetry upload is successful and in appropriate format indicating that it is from T2 component");
	    LOGGER.info("8. Validate if INFO and ERROR messages are present in /rdklogs/logs/telemetry2_0.txt.0 ");
	    LOGGER.info("9. Validate if dca_utility.sh is not listed under cron jobs ");
	    LOGGER.info("10. Disable the telemetry 2.0 via webpa and reboot ");
	    LOGGER.info("11. Verify if the 'telemetry2_0' process is up after enabling telemetry 2.0 ");
	    LOGGER.info("12. Validate if dca_utility.sh is listed under cron jobs ");
	    LOGGER.info("13. Validate the presence of logs in dcmscript.log ");
	    LOGGER.info("14. Verify log upload in splunk by searching using estb mac ");
	    LOGGER.info("Post-condition : SET THE DEVICE BACK TO TELEMETRY VERSION 2 SETTINGS  ");
	    LOGGER.info("#######################################################################################");

	    if (!isTelemetryVerTwoProcessUp) {
		{
		    throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
			    + "PRE_CONDITION_FAILED :Telemetry 2 process is not running on the gateway "
			    + device.getHostMacAddress());
		}
	    }

	    /**
	     * Steps 1 to 7
	     */
	    executeCommonStepsForTelemetry2(device, stepNumber, testCaseId, tapEnv);

	    /**
	     * Steps 8 to 14
	     */
	    stepNumber = 8;
	    BroadBandTelemetry2Utils.executeTestStepsForTelemetryVersionOne(device, tapEnv, stepNumber, testCaseId);
	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-TELEMETRY-VER2-1005");
    }

    /**
     * Method to execute common test steps for telemetry 2.0 scenarios
     * 
     * @param device
     *            {@link Instanceof Dut}
     * @param stepNumber
     *            step number to start with
     * @param testCaseId
     *            Test case ID to update
     * @param tapEnv
     *            {@link Instanceof AutomaticsTapApi}
     */
    public void executeCommonStepsForTelemetry2(Dut device, int stepNumber, String testCaseId,
	    AutomaticsTapApi tapEnv) {
	boolean status = false;
	String errorMessage = null;
	String response = null;
	long boxUpTimeInSeconds = 0;
	long boxUpTimeInSecondsAfterReboot = 0;
	boolean isSTBAccessible = false;
	/**
	 * VERIFY IF THE 'TELEMETRY2_0' PROCESS IS UP AFTER ENABLING TELEMETRY 2.0
	 */
	stepNum = "S" + stepNumber;
	errorMessage = "The process for telemetry 2 is not running on the gateway";
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP " + stepNumber
		+ " :DESCRIPTION : Verify if the 'telemetry2_0' process is up after enabling telemetry 2.0 ");
	LOGGER.info("STEP " + stepNumber + " :ACTION : Execute the command 'ps -ww | grep telemetry2_0 ");
	LOGGER.info("STEP " + stepNumber + " : EXPECTED : The process should be up and running. ");
	LOGGER.info("**********************************************************************************");

	status = BroadBandCommonUtils.verifyProcessRunningStatus(device, tapEnv, false,
		BroadBandTestConstants.PROCESS_NAME_TELEMETRY_2_0);

	if (status) {
	    LOGGER.info("STEP " + stepNum + ": ACTUAL : Telemetry 2.0 process is up and running on the gateway");
	} else {
	    LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	/**
	 * VERIFY IF THE LOG FILE /rdklogs/logs/telemetry2_0.txt.0 IS PRESENT IN ARM CONSOLE
	 */
	++stepNumber;
	stepNum = "S" + stepNumber;
	status = false;
	errorMessage = "The file /rdklogs/logs/telemetry2_0.txt.0 is not present on the device";
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP " + stepNumber
		+ " :DESCRIPTION : Verify if the log file /rdklogs/logs/telemetry_2.txt.0 is present in Arm console ");
	LOGGER.info(
		"STEP " + stepNumber + " :ACTION : Execute the command , 'ls -lrt /rdklogs/logs/telemetry2_0.txt.0' ");
	LOGGER.info("STEP " + stepNumber
		+ " : EXPECTED : The file /rdklogs/logs/telemetry2_0.txt.0 should be present on the device ");
	LOGGER.info("**********************************************************************************");

	status = BroadBandCommonUtils.doesFileExistWithinGivenTimeFrameInArm(tapEnv, device,
		BroadBandTestConstants.FILE_PATH_TELEMETRY_2_0, BroadBandTestConstants.TEN_MINUTE_IN_MILLIS,
		BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	if (status) {
	    LOGGER.info("STEP " + stepNum
		    + ": ACTUAL : The file /rdklogs/logs/telemetry2_0.txt.0 is present on the device");
	} else {
	    LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	/**
	 * VERIFY IF 'DCMSCRIPT.LOG' INDICATES T2 IS ENABLED
	 */
	++stepNumber;
	stepNum = "S" + stepNumber;
	status = false;
	errorMessage = "T2 enabled logs are not seen in /rdklogs/logs/dcmscript.log";
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP " + stepNumber + " :DESCRIPTION : Verify if 'dcmscript.log' indicates T2 is enabled ");
	LOGGER.info("STEP " + stepNumber
		+ " :ACTION : Execute the command , 'cat /rdklogs/logs/dcmscript.log' on the device ");
	LOGGER.info("STEP " + stepNumber + " : EXPECTED : The expected logs should be present on the file ");
	LOGGER.info("**********************************************************************************");

	response = tapEnv.executeCommandUsingSsh(device, BroadBandTestConstants.CMD_CAT_RDKLOGS_DCMSCRIPT_LOG);
	status = CommonMethods.isNotNull(response)
		&& CommonMethods.patternMatcher(response, BroadBandTestConstants.PATTERN_MATCHER_T2_IMPLEMENTATION);
	if (status) {
	    LOGGER.info("STEP " + stepNum
		    + ": ACTUAL : The file dcmscript.log indicates that T2 implementation is enabled on the device");
	} else {
	    if (CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv)) {
		boxUpTimeInSecondsAfterReboot = (CommonUtils.getBoxUptimeInSeconds(device, tapEnv) * 1000)
			+ BroadBandTestConstants.TEN_MINUTE_IN_MILLIS;
		do {
		    isSTBAccessible = CommonMethods.isSTBAccessible(device);
		    if (!isSTBAccessible) {
			LOGGER.error("Device is not accessible while checking dcm scipt.log");
		    }
		    boxUpTimeInSeconds = CommonUtils.getBoxUptimeInSeconds(device, tapEnv);
		    response = tapEnv.executeCommandUsingSsh(device,
			    BroadBandTestConstants.CMD_CAT_RDKLOGS_DCMSCRIPT_LOG);
		    status = CommonMethods.isNotNull(response) && CommonMethods.patternMatcher(response,
			    BroadBandTestConstants.PATTERN_MATCHER_T2_IMPLEMENTATION);
		} while (!status && (boxUpTimeInSeconds * 1000 < boxUpTimeInSecondsAfterReboot) && isSTBAccessible
			&& BroadBandCommonUtils.hasWaitForDuration(tapEnv,
				BroadBandTestConstants.FIFTEEN_SECONDS_IN_MILLIS));
		if (status) {
		    LOGGER.info("STEP " + stepNum
			    + ": ACTUAL : The file dcmscript.log indicates that T2 implementation is enabled on the device");
		} else {
		    LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
		}
	    } else {
		LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
	    }

	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	boolean isSplunkEnabled = BroadbandPropertyFileHandler.isSplunkEnabled();

	if (isSplunkEnabled) {

	    /**
	     * VERIFY IF T2 MARKERS ARE GETTING UPLOADED TO SPLUNK
	     */
	    ++stepNumber;
	    stepNum = "S" + stepNumber;
	    status = false;
	    errorMessage = "Telemetry 2 markers are not getting uploaded to splunk";
	    long startTimeStamp = System.currentTimeMillis();
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + " :DESCRIPTION : Verify if T2 markers are getting uploaded to splunk ");
	    LOGGER.info("STEP " + stepNumber
		    + " :ACTION : Execute the command , 'cat /rdklogs/logs/telemetry2_0.txt.0' on the device ");
	    LOGGER.info("STEP " + stepNumber
		    + " : EXPECTED : The logs like 'Report Sent Successfully over HTTP : 200' should present in /rdklogs/logs/telemetry2_0.txt.0 ");
	    LOGGER.info("**********************************************************************************");

	    do {
		response = tapEnv.executeCommandUsingSsh(device,
			BroadBandTestConstants.CMD_GET_REPORT_SENT_SUCCESS_MESSAGE_FROM_LOGS);
		status = CommonMethods.isNotNull(response)
			&& response.contains(BroadBandTestConstants.STRING_REPORT_SENT);
	    } while (!status
		    && (System.currentTimeMillis() - startTimeStamp) < BroadBandTestConstants.SIXTEEN_MINUTES_IN_MILLIS
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

	    if (status) {
		LOGGER.info("STEP " + stepNum
			+ ": ACTUAL : Telemetry 2 markers are getting uploaded to splunk successfully");
	    } else {
		LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    /**
	     * VERIFY IF LOGS ARE PRESENT IN SPLUNK
	     */

	    ++stepNumber;
	    stepNum = "S" + stepNumber;
	    status = false;
	    errorMessage = "Telemetry 2 logs are not present in splunk";
	    startTimeStamp = System.currentTimeMillis();
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + " :DESCRIPTION : Verify if T2 logs are present in splunk ");
	    LOGGER.info("STEP " + stepNumber
		    + " :ACTION : Perform search on splunk portal with the device's estb mac and the log upload time ");
	    LOGGER.info(
		    "STEP " + stepNumber + " : EXPECTED : The logs should be present with the appropriate timestamp ");
	    LOGGER.info("**********************************************************************************");

	    do {
		response = BroadBandTelemetry2Utils.retrieveTelemetryLogsFromSplunk(device, tapEnv, null);
		status = CommonMethods.isNotNull(response);
	    } while (!status
		    && (System.currentTimeMillis() - startTimeStamp) < BroadBandTestConstants.SIXTEEN_MINUTES_IN_MILLIS
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

	    if (status) {
		LOGGER.info("STEP " + stepNum
			+ ": ACTUAL : Telemetry 2 markers are getting uploaded to splunk successfully");
	    } else {
		LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * VALIDATE IF THE SPLUNK LOGS MANDATORY PARAMETERS
	     */

	    ++stepNumber;
	    stepNum = "S" + stepNumber;
	    status = false;
	    int count = 0;
	    errorMessage = "Mandatory parameters are not present in splunk log upload";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + " :DESCRIPTION : Validate if the splunk logs mandatory parameters ");
	    LOGGER.info("STEP " + stepNumber
		    + " :ACTION : Perform search on Splunk using the device eSTB MAC and get the latest payload data ");
	    LOGGER.info("STEP " + stepNumber
		    + " : EXPECTED : The mandatory parameters should be present in the log uploaded ");
	    LOGGER.info("**********************************************************************************");
	    for (String valueToBeValidate : BroadBandTestConstants.PATTERN_TELEMETRY_LOG_UPLOAD) {
		if (CommonUtils.patternSearchFromTargetString(response, valueToBeValidate)) {
		    count += BroadBandTestConstants.CONSTANT_1;
		}
	    }
	    status = count == BroadBandTestConstants.PATTERN_TELEMETRY_LOG_UPLOAD.size();
	    LOGGER.info("count " + count + " " + BroadBandTestConstants.PATTERN_TELEMETRY_LOG_UPLOAD.size());

	    if (status) {
		LOGGER.info("STEP " + stepNum
			+ ": ACTUAL : Mandatory parameters are present in splunk log upload. Log uploaded: "
			+ response);
	    } else {
		LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * VERIFY FROM THE SPLUNK THAT TELEMETRY UPLOAD IS SUCCESSFUL AND IN APPROPRIATE FORMAT INDICATING THAT IT
	     * IS FROM T2 COMPONENT
	     */

	    ++stepNumber;
	    stepNum = "S" + stepNumber;
	    status = false;
	    errorMessage = "The splunk logs doesnt have indication for T2 agent";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + " :DESCRIPTION : Verify from the splunk that  telemetry upload is successful and in appropriate format indicating that it is from T2 component ");
	    LOGGER.info("STEP " + stepNumber
		    + " :ACTION : Perform search on Splunk using the device eSTB MAC and get the latest payload data ");
	    LOGGER.info("STEP " + stepNumber
		    + " : EXPECTED : The splunk logs should indicate that it is from telemetry 2 ");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(response) && CommonMethods.patternMatcher(response,
		    BroadBandTestConstants.PATTERN_MATCHER_TELEMETRY_VERSION_2);

	    if (status) {
		LOGGER.info("STEP " + stepNum + ": ACTUAL : The splunk logs have indication for T2 agent: " + response);
	    } else {
		LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    
	} else {
	    for (int i = 0; i < 4; i++) {
	    LOGGER.info("SPLUNK is not enabled : Skipping the corresponding steps");
	    errorMessage = "splunk is disabled";
	    tapEnv.updateExecutionForAllStatus(device, testCaseId, "S"+(++stepNumber), ExecutionStatus.NOT_APPLICABLE,
		    errorMessage, false);    
	    }
	}

    }

    @AfterMethod
    public void executePostConditionForTelemetryVerTwo(Object[] params) {
	Dut device = (Dut) params[0];
	boolean status = false;
	if (isTelemetryVerOneEnabled && null != params[0]) {
	    LOGGER.debug("STARTING AFTER METHOD FOR TELEMETRY 2.0 ");
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS FOR TELEMETRY 2.0 ###################");
	    BroadBandPostConditionUtils.PostConditionToUpdateTelemetryVer1SettingsViaRFC(device, 1, tapEnv);
	    if (BroadBandCommonUtils.verifyProcessRunningStatus(device, tapEnv, false,
		    BroadBandTestConstants.PROCESS_NAME_TELEMETRY_2_0)) {
		status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv)
			&& !BroadBandCommonUtils.verifyProcessRunningStatus(device, tapEnv, false,
				BroadBandTestConstants.PROCESS_NAME_TELEMETRY_2_0);
		if (status) {
		    LOGGER.info("Telemetry 1.0 settings are updated successfully on the gateway");
		} else {
		    LOGGER.error("Telemetry 1.0 settings are not updated successfully on the gateway");
		}
	    }
	    LOGGER.info("################### ENDING POST-CONFIGURATIONS ###################");
	    LOGGER.debug("ENDING AFTER METHOD FOR TELEMETRY 2.0 ");
	} else {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("POST-CONDITION : DESCRIPTION : SET THE DEVICE BACK TO TELEMETRY VERSION 2 SETTINGS");
	    LOGGER.info("POST-CONDITION : ACTION : SET THE TELEMETRY SETTINGS VIA WEBPA FOR TELEMETRY VERSION 2");
	    LOGGER.info("POST-CONDITION : EXPECTED : THE SETTINGS SHOULD BE SET SUCCESSFULLY");
	    LOGGER.info("#######################################################################################");
	    try {
		String response = null;
		long boxUpTimeInSeconds = 0;
		long boxUpTimeInSecondsAfterReboot = 0;
		boolean isSTBAccessible = false;
		/**
		 * EXECUTING POST-CONDITION FOR TELEMETRY 2.0
		 */
		LOGGER.info("################### STARTING POST-CONFIGURATIONS FOR TELEMETRY 2.0 ###################");
		LOGGER.info("PRE-CONDITION STEPS");
		LOGGER.info("##################################################################################");
		LOGGER.info("POST-CONDITION "
			+ " :DESCRIPTION : VALIDATE IF DEVICE IS ON TELEMETRY 2.0 SETTINGS. ELSE, SET TELEMETRY 2.0 SETTINGS ON GATEWAY VIA RFC");
		LOGGER.info("POST-CONDITION "
			+ " :ACTION : EXECUTE THE COMMAND PS|GREP -I TELEMETRY AND CHECK IF TELEMETRY 2.0 PROCESS IS UP AND RUNNING. ELSE SET TELEMETRY 2.0 SETTINGS ");
		LOGGER.info("POST-CONDITION "
			+ " : EXPECTED : THE DEVICE SHOULD BE SUCCESSFULLY UPGRADED TO TELEMETRY 2.0 SETTINGS ");
		LOGGER.info("##################################################################################");

		status = BroadBandCommonUtils.verifyProcessRunningStatus(device, tapEnv, false,
			BroadBandTestConstants.PROCESS_NAME_TELEMETRY_2_0);
		if (!status) {
		    response = tapEnv.executeWebPaCommand(device,
			    BroadBandWebPaConstants.WEBPA_PARAM_FOR_TELEMETRY_2_0_VERSION);
		    if (CommonMethods.isNotNull(response)) {
			isTelemetryVerOneEnabled = response.equals(BroadBandTestConstants.STRING_VALUE_ONE);
		    }
		    // check the t2 version from stb.properties and update the enum

		    String t2version = BroadBandTelemetry2Utils.validateT2Version(device, tapEnv);
		    LOGGER.info("T2 version from stb.properties: " + t2version);
		    BroadBandTestConstants.TELEMETRY_2_WEBPA_SETTINGS.VERSION.setDefaultT2Config(t2version);

		    if (BroadBandRfcFeatureControlUtils.executePreconditionForRfcTests(device, tapEnv,
			    AutomaticsTapApi.getSTBPropsValue(BroadBandTestConstants.PROP_KEY_TELEMETRY_VER_2_CONFIG)
				    .replaceAll(BroadBandTestConstants.TELEMETRY_ENABLE_VAUE,
					    BroadBandTestConstants.TELEMETRY_2_WEBPA_SETTINGS.FEATURE_ENABLE
						    .getDefaultT2Config())
				    .replaceAll(BroadBandTestConstants.TELEMETRY_VERSION_VALUE,
					    BroadBandTestConstants.TELEMETRY_2_WEBPA_SETTINGS.VERSION
						    .getDefaultT2Config())
				    .replaceAll(BroadBandTestConstants.TELEMETRY_CONFIG_URL_VALUE,
					    BroadBandTestConstants.TELEMETRY_2_WEBPA_SETTINGS.CONFIG_URL
						    .getDefaultT2Config()))) {
			status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_RFC_CONTROL,
				BroadBandTestConstants.STRING_CONSTANT_1,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
				BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
			if (!status) {
			    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
				    BroadBandWebPaConstants.WEBPA_PARAM_RFC_CONTROL, BroadBandTestConstants.CONSTANT_2,
				    BroadBandTestConstants.STRING_CONSTANT_1,
				    BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
				    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			}
			if (CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv)) {
			    status = BroadBandRfcFeatureControlUtils
				    .verifyParameterUpdatedByRfc(device, tapEnv,
					    BroadBandTestConstants.TELEMETRY_2_WEBPA_SETTINGS.FEATURE_ENABLE.getParam(),
					    BroadBandTestConstants.TELEMETRY_2_WEBPA_SETTINGS.FEATURE_ENABLE
						    .getDefaultT2Config())
				    .isStatus()
				    && BroadBandRfcFeatureControlUtils.verifyParameterUpdatedByRfc(device, tapEnv,
					    BroadBandTestConstants.TELEMETRY_2_WEBPA_SETTINGS.CONFIG_URL.getParam(),
					    BroadBandTestConstants.TELEMETRY_2_WEBPA_SETTINGS.CONFIG_URL
						    .getDefaultT2Config())
					    .isStatus()
				    && BroadBandRfcFeatureControlUtils.verifyParameterUpdatedByRfc(device, tapEnv,
					    BroadBandTestConstants.TELEMETRY_2_WEBPA_SETTINGS.VERSION.getParam(),
					    BroadBandTestConstants.TELEMETRY_2_WEBPA_SETTINGS.VERSION
						    .getDefaultT2Config())
					    .isStatus()
				    && BroadBandTelemetry2Utils.verifyTelemetry2ConfigurationViaWebpa(device, tapEnv);
			    boxUpTimeInSecondsAfterReboot = (CommonUtils.getBoxUptimeInSeconds(device, tapEnv) * 1000)
				    + BroadBandTestConstants.EIGHT_MINUTE_IN_MILLIS;
			    do {
				isSTBAccessible = CommonMethods.isSTBAccessible(device);
				if (!isSTBAccessible) {
				    LOGGER.error(
					    "Device is not accessible while checking for telemetry process is up and running");
				}
				boxUpTimeInSeconds = CommonUtils.getBoxUptimeInSeconds(device, tapEnv);
				status = BroadBandCommonUtils.verifyProcessRunningStatus(device, tapEnv, false,
					BroadBandTestConstants.PROCESS_NAME_TELEMETRY_2_0);
			    } while (!status && (boxUpTimeInSeconds * 1000 < boxUpTimeInSecondsAfterReboot)
				    && isSTBAccessible && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
					    BroadBandTestConstants.FIFTEEN_SECONDS_IN_MILLIS));

			}
		    }
		    isTelemetryVerTwoProcessUp = status;

		} else {
		    LOGGER.info("THE DEVICE IS ALREADY IN TELEMETRY 2.0 CONFIGURATION");
		}

		if (!status) {
		    LOGGER.error("POST-CONDITION: DEVICE HAS NOT BEEN SET TO TELEMETRY 2 SETTINGS SUCCESSFULLY");
		}
	    } catch (Exception e) {
		LOGGER.error("EXCEPTION OCCURED WHILE EXECUTING PRE-CONDITION FOR TELEMETRY 2.0..", e);
	    }
	    LOGGER.info("################### ENDING POST-CONFIGURATIONS ###################");
	}
    }
    
    /**
	 * Verify telemetry 2 configurations persist over a device upgrade
	 * <ol>
	 * <li>Pre-condition : Validate if the T2 settings are already present else set
	 * the settings and reboot the device</li>
	 * <li>Verify if the 'telemetry2_0' process is up after enabling telemetry
	 * 2.0</li>
	 * <li>Verify if the log file /rdklogs/logs/telemetry_2.txt.0 is present in Arm
	 * console</li>
	 * <li>Verify if 'dcmscript.log' indicates T2 is enabled</li>
	 * <li>Verify if T2 markers are getting uploaded to splunk</li>
	 * <li>Verify if T2 logs are present in splunk</li>
	 * <li>Validate if the splunk logs mandatory parameters</li>
	 * <li>Verify from the splunk that telemetry upload is successful and in
	 * appropriate format indicating that it is from T2 component</li>
	 * <li>Upgrade the device to a release build</li>
	 * <li>Verify if the 'telemetry2_0' process is up after enabling telemetry
	 * 2.0</li>
	 * <li>Verify if the log file /rdklogs/logs/telemetry_2.txt.0 is present in Arm
	 * console</li>
	 * <li>Verify if 'dcmscript.log' indicates T2 is enabled</li>
	 * <li>Verify if T2 markers are getting uploaded to splunk</li>
	 * <li>Verify if T2 logs are present in splunk</li>
	 * <li>Validate if the splunk logs mandatory parameters</li>
	 * <li>Verify from the splunk that telemetry upload is successful and in
	 * appropriate format indicating that it is from T2 component</li>
	 * <li>Post-condition : Revert the image back to the initial image
	 * </ol>
	 * 
	 * @param device {@link Instanceof Dut}
	 *
	 * @author Sathurya Ravi
	 * @refactor yamini.s
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
	@TestDetails(testUID = "TC-RDKB-TELEMETRY-VER2-1003")
	public void testToVerifyTelemetryVersion2OnImageUpgrade(Dut device) {
		// Variable Declaration begins
		String testCaseId = "TC-RDKB-TELEMETRY-VER2-103";
		String errorMessage = null;
		int stepNumber = 1;
		boolean status = false;
		String initialFirmwareVersion = null;
		String latestImage = null;
		// Variable Declaration Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-TELEMETRY-VER2-1003");
		LOGGER.info("TEST DESCRIPTION: Verify telemetry 2 configurations persist over a device upgrade");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info(
				"Pre-condition : Validate if the T2 settings are already present else set the settings and reboot the device ");
		LOGGER.info("Verify if the 'telemetry2_0' process is up after enabling telemetry 2.0");
		LOGGER.info("Verify if the log file /rdklogs/logs/telemetry_2.txt.0 is present in Arm console");
		LOGGER.info("Verify if 'dcmscript.log' indicates T2 is enabled");
		LOGGER.info("Verify if T2 markers are getting uploaded to splunk");
		LOGGER.info("Verify if T2 logs are present in splunk");
		LOGGER.info("Validate if the splunk logs mandatory parameters");
		LOGGER.info(
				"Verify from the splunk that telemetry upload is successful and in appropriate format indicating that it is from T2 component");
		LOGGER.info("Upgrade the device to a release build");
		LOGGER.info("Verify if the 'telemetry2_0' process is up after enabling telemetry 2.0");
		LOGGER.info("Verify if the log file /rdklogs/logs/telemetry_2.txt.0 is present in Arm console");
		LOGGER.info("Verify if 'dcmscript.log' indicates T2 is enabled");
		LOGGER.info("Verify if T2 markers are getting uploaded to splunk");
		LOGGER.info("Verify if T2 logs are present in splunk");
		LOGGER.info("Validate if the splunk logs mandatory parameters");
		LOGGER.info(
				"Verify from the splunk that telemetry upload is successful and in appropriate format indicating that it is from T2 component");
		LOGGER.info("Post-condition : Revert the image back to the initial image ");
		LOGGER.info("#######################################################################################");

		try {

			if (!isTelemetryVerTwoProcessUp) {
				{
					throw new TestException(RDKBTestConstants.PRE_CONDITION_ERROR
							+ "PRE_CONDITION_FAILED :Telemetry 2 process is not running on the gateway "
							+ device.getHostMacAddress());
				}
			}

			/**
			 * Steps 1 to 7
			 */
			executeCommonStepsForTelemetry2(device, stepNumber, testCaseId, tapEnv);

			stepNumber = 8;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = "Unable to check device status";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : VERIFY TRIGGERED CDL SUCCESSFUL WITH LATEST FIRMWARE VERSION");
			LOGGER.info(
					"STEP " + stepNumber + ": ACTION : TRIGGER THE CDL WITH LATEST FIRMWARE VERSION USING TR-181/SNMP");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : CDL MUST SUCCESSFUL WITH LATEST FIRMWARE VERSION.");
			LOGGER.info("**********************************************************************************");
			errorMessage = "UNABLE TO TRIGGER THE IMAGE UPGRADE ON THE DEVICE WITH LATEST FIRMWARE VERSION.";
			initialFirmwareVersion = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
			if (CommonMethods.isNotNull(initialFirmwareVersion)) {
				errorMessage = "Failed to get latest image from rdkportal for device model: " + device.getModel();

				latestImage = tapEnv.getLatestBuildImageVersionForCdlTrigger(device, false);
				LOGGER.info("LATEST FIRMWARE VERSION: " + latestImage);
				if (CommonMethods.isNull(latestImage)) {
					LOGGER.info(
							" GA image obtained from deployed version service is null. Hence getting the image from property file ");
					latestImage = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
							BroadBandPropertyKeyConstants.PARTIAL_PROPERTY_KEY_FOR_GA_IMAGE);
					LOGGER.info("Latest Firmware version from property file: " + latestImage);
				}

				if (CommonMethods.isNotNull(latestImage) && initialFirmwareVersion.equalsIgnoreCase(latestImage)) {

					latestImage = tapEnv.getLatestBuildImageVersionForCdlTrigger(device, false);
					LOGGER.info("LATEST FIRMWARE VERSION: " + latestImage);
					if (CommonMethods.isNull(latestImage)) {
						LOGGER.info(
								" GA image obtained from deployed version service is null. Hence getting the image from property file ");
						latestImage = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
								BroadBandPropertyKeyConstants.PARTIAL_PROPERTY_KEY_FOR_GA_IMAGE);
						LOGGER.info("Latest Firmware version from property file: " + latestImage);
					}
				}
				if (CommonMethods.isNotNull(latestImage)) {
					errorMessage = "Unable to trigger CDL for latest build - " + latestImage;
					status = FirmwareDownloadUtils.performXconfHttpImageUpgrade(tapEnv, device, latestImage);
				}
			}
			LOGGER.info("FLASHED THE LATEST BUILD ON THE DEVICE: " + status);

			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL :Successfully upgraded to latest version");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * Steps 9 to 13
			 */
			executeCommonStepsForTelemetry2(device, ++stepNumber, testCaseId, tapEnv);

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {

			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"POST-CONDITION 1 : DESCRIPTION : VERIFY TRIGGERED CDL SUCCESSFUL WITH EARLIER FIRMWARE VERSION");
			LOGGER.info("POST-CONDITION 1 : ACTION : TRIGGER THE CDL WITH EARLIER FIRMWARE VERSION USING TR-181/SNMP");
			LOGGER.info("POST-CONDITION 1 : EXPECTED : CDL MUST SUCCESSFUL WITH EARLIER FIRMWARE VERSION");
			LOGGER.info("#######################################################################################");

			status = BroadBandCodeDownloadUtils.triggerPreviousCodeDownload(device, tapEnv, initialFirmwareVersion);
			if (status) {
				LOGGER.info("POST-CONDITION: Build has been reverted back to original build successfully");
			} else {
				LOGGER.error("POST-CONDITION: Build has not been changed to previous build");
			}
			LOGGER.info("################### ENDING POST-CONFIGURATIONS ###################");

		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-TELEMETRY-VER2-1003");
	}

}
