
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

import java.util.List;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Device;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.BroadBandSystemUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandTelemetryPingTests extends AutomaticsTestBase {
	
	/**
     * 
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>PRE-CONDITION 1: Validate if the T2 settings are already present else set the settings and reboot the device
     * </li>
     * <li>STEP 1 : Verify if the 'telemetry2_0' process is up after enabling telemetry 2.0</li>
     * <li>STEP 2 : Verify if the log file /rdklogs/logs/telemetry_2.txt.0 is present in Arm console</li>
     * <li>STEP 3 : Verify if 'dcmscript.log' indicates T2 is enabled</li>
     * <li>Step 4 : Validate TR181 parameter Device.IP.Diagnostics.X_RDKCENTRAL-COM_PingTest.PartnerID.</li>
     * <li>Step 5 : Validate TR181 parameter Device.IP.Diagnostics.X_RDKCENTRAL-COM_PingTest.ecmMAC.</li>
     * <li>Step 6 : Validate TR181 parameter Device.IP.Diagnostics.X_RDKCENTRAL-COM_PingTest.DeviceID.</li>
     * <li>Step 7 : Validate TR181 parameter Device.IP.Diagnostics.X_RDKCENTRAL-COM_PingTest.DeviceModel.</li>
     * <li>Step 8 : Validate TR181 parameter Device.IP.Diagnostics.IPPing.Interface.</li>
     * <li>Step 9 : Validate Device.IP.Diagnostics.IPPing.Host.</li>
     * <li>Step 10 : Validate Device.IP.Diagnostics.X_RDKCENTRAL-COM_PingTest.Run.</li>
     * <li>Step 11: Validate TDMlog.txt.0 log file.</li>
     * <li>Step 12 : Validate TR181 parameter Device.IP.Diagnostics.IPPing.NumberOfRepetitions.</li>
     * <li>Step 13 : Validate TR181 parameter Device.IP.Diagnostics.IPPing.Timeout.</li>
     * <li>Step 14 : Validate TR181 parameter Device.IP.Diagnostics.IPPing.AverageResponseTime.</li>
     * <li>Step 15 : Validate if invalid url/host address is successfully set for TR181 param
     * Device.IP.Diagnostics.IPPing.Host.</li>
     * <li>Step 16 : Trigger ping test by passing invalid url or host address and Validate TR181 parameter
     * Device.IP.Diagnostics.IPPing.DiagnosticsState.</li>
     * <li>Step 17 : Validating TDMlog.txt.0 log file for invalid url/host address ping test.</li>
     * </ol>
     * 
     * @param device
     *            {@link Dut}
     * 
     * @author Susheela C
     * @refactor yamini.s
     * 
     * 
     */

    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.TELEMETRY)
    @TestDetails(testUID = "TC-RDKB-TELEMETRY-1500")
    public void testToGetNetworkLatencyAndMetadata(Dut device) {
	// Holds the test case ID
	String testCaseId = "TC-RDKB-TELEMETRY-150";
	// boolean variable to store the status
	boolean status = false;
	// Test step number
	String stepNumber = "s1";
	// Error message
	String errorMessage = null;
	// Variable to store the command built using string buffer
	String sshCommand = null;
	String hostAddress = null;
	String invalidHostAddress = null;
	String partnerId = null;
	String response = null;
	boolean isTelemetry2Enabled = false;
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE:TC-RDKB-TELEMETRY-1500");
	LOGGER.info("TEST DESCRIPTION: To Collect network latency and metadata.");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info(
		"PRE-CONDITION 1: Validate if the T2 settings are already present else set the settings and reboot the device");
	LOGGER.info("STEP 1 : Verify if the 'telemetry2_0' process is up after enabling telemetry 2.0");
	LOGGER.info("STEP 2 : Verify if the log file /rdklogs/logs/telemetry_2.txt.0 is present in Arm console");
	LOGGER.info("STEP 3 : Verify if 'dcmscript.log' indicates T2 is enabled ");
	LOGGER.info("STEP 4. Validate TR181 parameter Device.IP.Diagnostics.X_RDKCENTRAL-COM_PingTest.PartnerID.");
	LOGGER.info("STEP 5. Validate TR181 parameter Device.IP.Diagnostics.X_RDKCENTRAL-COM_PingTest.ecmMAC.");
	LOGGER.info("STEP 6. Validate TR181 parameter Device.IP.Diagnostics.X_RDKCENTRAL-COM_PingTest.DeviceID.");
	LOGGER.info("STEP 7. Validate TR181 parameter Device.IP.Diagnostics.X_RDKCENTRAL-COM_PingTest.DeviceModel.");
	LOGGER.info("STEP 8. Validate TR181 parameter Device.IP.Diagnostics.IPPing.Interface.");
	LOGGER.info("STEP 9. Validate Device.IP.Diagnostics.IPPing.Host.");
	LOGGER.info("STEP 10. Validate  Device.IP.Diagnostics.X_RDKCENTRAL-COM_PingTest.Run.");
	LOGGER.info("STEP 11. Validate TDMlog.txt.0 log file.");
	LOGGER.info("STEP 12. Validate TR181 parameter Device.IP.Diagnostics.IPPing.NumberOfRepetitions.");
	LOGGER.info("STEP 13. Validate TR181 parameter Device.IP.Diagnostics.IPPing.Timeout.");
	LOGGER.info("STEP 14. Validate TR181 parameter Device.IP.Diagnostics.IPPing.AverageResponseTime.");
	LOGGER.info(
		"15. Validate  if invalid url/host address is successfully set for TR181 param Device.IP.Diagnostics.IPPing.Host.");
	LOGGER.info(
		"16. Trigger ping test by passing invalid url or host address and Validate TR181 parameter Device.IP.Diagnostics.IPPing.DiagnosticsState.");
	LOGGER.info("17. Validating TDMlog.txt.0 log file for invalid url/host address ping test.");

	try {
	    /**
	     * Executing pre-condition for telemetry version 2
	     */
	    isTelemetry2Enabled = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_TELEMETRY_2_0_ENABLE, BroadBandTestConstants.TRUE,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS, BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
	    if (isTelemetry2Enabled) {
		LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
		LOGGER.info("PRE-CONDITION STEPS");
		BroadBandPreConditionUtils.executePreConfigurationsForTelemetry(device, tapEnv, 1);
	    }

	    // Variable to hold the Device Model
	    String deviceModelFromSettop = device.getModel();
	    // Variable to hold the ecmMac
	    String ecmMacFromBHC = ((Device) device).getEcmMac();
	    // Variable to hold the DeviceId
	    String deviceIdFromSettop = device.getSerialNumber();

	    /**
	     * VERIFY IF THE 'TELEMETRY2_0' PROCESS IS UP AFTER ENABLING TELEMETRY 2.0
	     */
	    stepNumber = "s1";

	    errorMessage = "The process for telemetry 2 is not running on the gateway";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1 :DESCRIPTION : Verify if the 'telemetry2_0' process is up after enabling telemetry 2.0 ");
	    LOGGER.info("STEP 1 :ACTION : Execute the command 'ps -ww | grep telemetry2_0 ");
	    LOGGER.info("STEP 1 : EXPECTED : The process should be up and running. ");
	    LOGGER.info("**********************************************************************************");
	    isTelemetry2Enabled = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_TELEMETRY_2_0_ENABLE, BroadBandTestConstants.TRUE,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS, BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
	    if (isTelemetry2Enabled) {
		status = BroadBandCommonUtils.verifyProcessRunningStatus(device, tapEnv, false,
			BroadBandTestConstants.PROCESS_NAME_TELEMETRY_2_0);

		if (status) {
		    LOGGER.info("STEP 1 : ACTUAL : Telemetry 2.0 process is up and running on the gateway");
		} else {
		    LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
	    } else {
		LOGGER.info("STEP 1 IS NOT APPLICABLE IF TELEMETRY 2 IS NOT ENABLED");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }
	    /**
	     * VERIFY IF THE LOG FILE /rdklogs/logs/telemetry2_0.txt.0 IS PRESENT IN ARM CONSOLE
	     */
	    stepNumber = "s2";

	    status = false;
	    errorMessage = "The file /rdklogs/logs/telemetry2_0.txt.0 is not present on the device";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 2 :DESCRIPTION : Verify if the log file /rdklogs/logs/telemetry_2.txt.0 is present in Arm console ");
	    LOGGER.info("STEP 2 :ACTION : Execute the command , 'ls -lrt /rdklogs/logs/telemetry2_0.txt.0' ");
	    LOGGER.info(
		    "STEP 2 : EXPECTED : The file /rdklogs/logs/telemetry2_0.txt.0 should be present on the device ");
	    LOGGER.info("**********************************************************************************");
	    if (isTelemetry2Enabled) {
		status = BroadBandCommonUtils.doesFileExistWithinGivenTimeFrameInArm(tapEnv, device,
			BroadBandTestConstants.FILE_PATH_TELEMETRY_2_0, BroadBandTestConstants.TEN_MINUTE_IN_MILLIS,
			BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		if (status) {
		    LOGGER.info("STEP 2: ACTUAL : The file /rdklogs/logs/telemetry2_0.txt.0 is present on the device");
		} else {
		    LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
	    } else {
		LOGGER.info("STEP 2 IS NOT APPLICABLE IF TELEMETRY 2 IS NOT ENABLED");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }
	    /**
	     * VERIFY IF 'DCMSCRIPT.LOG' INDICATES T2 IS ENABLED
	     */
	    stepNumber = "s3";
	    status = false;
	    errorMessage = "T2 enabled logs are not seen in /rdklogs/logs/dcmscript.log";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3 :DESCRIPTION : Verify if 'dcmscript.log' indicates T2 is enabled ");
	    LOGGER.info("STEP 3 :ACTION : Execute the command , 'cat /rdklogs/logs/dcmscript.log' on the device ");
	    LOGGER.info("STEP 3 : EXPECTED : The expected logs should be present on the file ");
	    LOGGER.info("**********************************************************************************");
	    if (isTelemetry2Enabled) {
		try {
		    response = tapEnv.executeCommandUsingSsh(device,
			    BroadBandTestConstants.CMD_CAT_RDKLOGS_DCMSCRIPT_LOG);
		} catch (Exception exception) {
		    errorMessage = "Unable to execute command in device" + exception.getMessage();
		    LOGGER.error(errorMessage);
		}
		status = CommonMethods.isNotNull(response) && CommonMethods.patternMatcher(response,
			BroadBandTestConstants.PATTERN_MATCHER_T2_IMPLEMENTATION);
		if (status) {
		    LOGGER.info(
			    "STEP 3: ACTUAL : The file dcmscript.log indicates that T2 implementation is enabled on the device");
		} else {
		    LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    } else {
		LOGGER.info("STEP 3 IS NOT APPLICABLE IF TELEMETRY 2 IS NOT ENABLED");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }
	    /**
	     * Step 4 : Validate TR181 parameter Device.IP.Diagnostics.X_RDKCENTRAL-COM_PingTest.PartnerID.
	     */
	    stepNumber = "s4";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_SYNDICATION_PARTNER_ID);
	    List<String> partnersList = null;
	    try {
		partnersList = BroadbandPropertyFileHandler.getPartnerListByResolvingPlatform(device);
	    } catch (Exception e) {
		LOGGER.info("PartnerId list is empty or null :" + e.getMessage());
	    }
	    if (CommonMethods.isNotNull(response) && partnersList != null && partnersList.contains(response)) {
		partnerId = response.trim();
	    } else {
		partnerId = BroadbandPropertyFileHandler.getDefaultPartnerID();
	    }

	    errorMessage = "Failed to return the expected PartnerID";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 4 : DESCRIPTION : Validate TR181 parameter Device.IP.Diagnostics.X_RDKCENTRAL-COM_PingTest.PartnerID");
	    LOGGER.info(
		    "STEP 4 : ACTION : EXECUTE THE WEBPA COMMAND : Device.IP.Diagnostics.X_RDKCENTRAL-COM_PingTest.PartnerID.");
	    LOGGER.info("STEP 4 : EXPECTED : The PartnerId should be returned as value " + partnerId);
	    LOGGER.info("**********************************************************************************");
	    try {
		status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_PARTNERID, partnerId);
	    } catch (Exception e) {
		errorMessage = e.getMessage();
		LOGGER.error("Exception occurred while validating partnerId" + errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP 4 : ACTUAL : The PartnerId returned the value as " + partnerId);
	    } else {
		LOGGER.error("STEP 4 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	    /**
	     * Step 5 : Validate TR181 parameter Device.IP.Diagnostics.X_RDKCENTRAL-COM_PingTest.ecmMAC.
	     */
	    stepNumber = "s5";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 5 : DESCRIPTION : Validate TR181 parameter Device.IP.Diagnostics.X_RDKCENTRAL-COM_PingTest.ecmMAC");
	    LOGGER.info(
		    "STEP 5 : ACTION : EXECUTE THE WEBPA COMMAND : Device.IP.Diagnostics.X_RDKCENTRAL-COM_PingTest.ecmMAC.");
	    LOGGER.info("STEP 5 : EXPECTED : The EcmMac should be returned");
	    LOGGER.info("**********************************************************************************");
	    try {
		status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_ECM_MAC, ecmMacFromBHC);
	    } catch (Exception e) {
		errorMessage = e.getMessage();
		LOGGER.error("Exception occurred while validating ecmMac" + errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP 5 : ACTUAL : The EcmMac obtained successfully");
	    } else {
		LOGGER.error("STEP 5 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	    /**
	     * Step 6 : Validate TR181 parameter Device.IP.Diagnostics.X_RDKCENTRAL-COM_PingTest.DeviceID.
	     */
	    stepNumber = "s6";
	    status = false;
	    errorMessage = "The DeviceId not obtained";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 6 : DESCRIPTION : Validate TR181 parameter Device.IP.Diagnostics.X_RDKCENTRAL-COM_PingTest.DeviceID");
	    LOGGER.info(
		    "STEP 6 : ACTION : EXECUTE THE WEBPA COMMAND : Device.IP.Diagnostics.X_RDKCENTRAL-COM_PingTest.DeviceID.");
	    LOGGER.info("STEP 6 : EXPECTED : The DeviceId should be returned");
	    LOGGER.info("**********************************************************************************");
	    try {
		status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_DEVICE_ID, deviceIdFromSettop);
	    } catch (Exception e) {
		errorMessage = e.getMessage();
		LOGGER.error("Exception occurred while validating DeviceID" + errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP 6 : ACTUAL : The DeviceId obtained successfully");
	    } else {
		LOGGER.error("STEP 6 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	    /**
	     * Step 7 : Validate TR181 parameter Device.IP.Diagnostics.X_RDKCENTRAL-COM_PingTest.DeviceModel.
	     */
	    stepNumber = "s7";
	    status = false;
	    String deviceModel = null;
	    errorMessage = "The DeviceModel obtained is-" + deviceModel + " expected model -" + deviceModelFromSettop;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 7 : DESCRIPTION : Validate TR181 parameter Device.IP.Diagnostics.X_RDKCENTRAL-COM_PingTest.DeviceModel");
	    LOGGER.info(
		    "STEP 7 : ACTION : EXECUTE THE WEBPA COMMAND : Device.IP.Diagnostics.X_RDKCENTRAL-COM_PingTest.DeviceModel.");
	    LOGGER.info("STEP 7 : EXPECTED : The DeviceModel should be returned");
	    LOGGER.info("**********************************************************************************");
	    try {
		status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_DEVICE_MODEL, deviceModelFromSettop);
	    } catch (Exception e) {
		errorMessage = e.getMessage();
		LOGGER.error("Exception occurred while validating DeviceModel" + errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP 7 : ACTUAL : The DeviceModel obtained successfully");
	    } else {
		LOGGER.error("STEP 7 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	    /**
	     * Step 8 : Validate TR181 parameter Device.IP.Diagnostics.IPPing.Interface.
	     */

	    stepNumber = "s8";
	    status = false;
	    errorMessage = "TR181 parameter Device.IP.Diagnostics.IPPing.Interface not passed";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 8 : DESCRIPTION : Validate TR181 parameter Device.IP.Diagnostics.IPPing.Interface");
	    LOGGER.info("STEP 8 : ACTION : EXECUTE THE WEBPA COMMAND : Device.IP.Diagnostics.IPPing.Interface.");
	    LOGGER.info("STEP 8 : EXPECTED : Interface should be set as 'erouter0' successfully");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_SET_INTERFACE, BroadBandTestConstants.CONSTANT_0,
		    BroadBandTestConstants.INTERFACE_NAME_EROUTER0);
	    if (status) {
		LOGGER.info(
			"STEP 8 : ACTUAL : TR181 parameter Device.IP.Diagnostics.IPPing.Interface passed successfully");
	    } else {
		LOGGER.error("STEP 8 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	    /**
	     * Step 9 : Validate Device.IP.Diagnostics.IPPing.Host.
	     */
	    stepNumber = "s9";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 9 : DESCRIPTION : Validate TR181 parameter Device.IP.Diagnostics.IPPing.Host");
	    LOGGER.info("STEP 9 : ACTION : EXECUTE THE WEBPA COMMAND : Device.IP.Diagnostics.IPPing.Host.");
	    LOGGER.info("STEP 9 : EXPECTED : Host address should be passed successfully");
	    LOGGER.info("**********************************************************************************");
	    try {
		errorMessage = "Unable to get the Host address for ping test";
		hostAddress = AutomaticsTapApi
			.getSTBPropsValue(BroadBandTestConstants.PROP_KEY_HOST_ADDRESS_FOR_PINGTEST);
	    } catch (Exception e) {
		LOGGER.error("Key not found on stb.properties : "
			+ BroadBandTestConstants.PROP_KEY_HOST_ADDRESS_FOR_PINGTEST);
		hostAddress = BroadBandTestConstants.STRING_GOOGLE_HOST_ADDRESS;
	    }
	    /**
	     * The default url configured in "stb.properties" for "pingtest.host.address" is : www.google.com To ping
	     * other urls, please update the value of "pingtest.host.address" in "stb.properties"
	     **/
	    if (CommonMethods.isNotNull(hostAddress)) {
		errorMessage = "Host address not passed";
		status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_TO_SET_HOST_ADDRESS, BroadBandTestConstants.CONSTANT_0,
			hostAddress);
	    }
	    if (status) {
		LOGGER.info("STEP 9 : ACTUAL : Host address passed successfully");
	    } else {
		LOGGER.error("STEP 9 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	    /**
	     * Step 10 : Validate Device.IP.Diagnostics.X_RDKCENTRAL-COM_PingTest.Run.
	     */
	    stepNumber = "s10";
	    status = false;
	    String deviceDateTime = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
	    errorMessage = "PingTest not Triggered";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 10 : DESCRIPTION : Validate TR181 parameter Device.IP.Diagnostics.X_RDKCENTRAL-COM_PingTest.Run");
	    LOGGER.info(
		    "STEP 10 : ACTION : EXECUTE THE WEBPA COMMAND : Device.IP.Diagnostics.X_RDKCENTRAL-COM_PingTest.Run.");
	    LOGGER.info("STEP 10 : EXPECTED : PingTest should be Triggered successfully");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_TRIGGER_PINGTEST, BroadBandTestConstants.CONSTANT_3,
		    BroadBandTestConstants.TRUE);
	    if (status) {
		LOGGER.info("STEP 10 : ACTUAL : PingTest Triggered successfully");
	    } else {
		LOGGER.error("STEP 10 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	    /**
	     * Step 11 : Validate TDMlog.txt.0 log file.
	     */
	    stepNumber = "s11";
	    status = false;
	    errorMessage = "The logs related to pingtest results not obtained";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 11 : DESCRIPTION : Validating TDMlog.txt.0 log file");
	    LOGGER.info(
		    "STEP 11 : ACTION : EXECUTE THE COMMAND : grep -A 1 -i \"DeviceId\" /rdklogs/logs/TDMlog.txt.0 | tail -2.");
	    LOGGER.info("STEP 11 : EXPECTED : The logs related to pingtest results must be obtained");
	    LOGGER.info("**********************************************************************************");
	    sshCommand = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.TEXT_DOUBLE_QUOTE,
		    BroadBandTraceConstants.LOG_MESSAGE_DEVICE_ID, deviceIdFromSettop,
		    BroadBandTestConstants.SEMI_COLON, BroadBandTraceConstants.LOG_MESSAGE_ECM_MAC, ecmMacFromBHC,
		    BroadBandTestConstants.SEMI_COLON, BroadBandTraceConstants.LOG_MESSAGE_PARTNER_ID, partnerId,
		    BroadBandTestConstants.SEMI_COLON, BroadBandTraceConstants.LOG_MESSAGE_DEVICE_MODEL,
		    deviceModelFromSettop, BroadBandTestConstants.SEMI_COLON,
		    BroadBandTraceConstants.LOG_MESSAGE_END_POINT, hostAddress,
		    BroadBandTestConstants.TEXT_DOUBLE_QUOTE);
	    status = BroadBandSystemUtils.verifyArmConsoleLog(tapEnv, device, sshCommand,
		    BroadBandTestConstants.LOG_FILE_PINGTEST, deviceDateTime);
	    if (!status) {
		LOGGER.info("Failed to get output in a single line ,hence trying with two lines");
		sshCommand = BroadBandCommonUtils.concatStringUsingStringBuffer(
			BroadBandCommandConstants.CMD_GREP_A_I.replaceAll(
				BroadBandTestConstants.STRING_VALUE_TO_REPLACE,
				BroadBandTestConstants.STRING_CONSTANT_1),
			BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandTraceConstants.LOG_MESSAGE_DEVICE_ID,
			deviceIdFromSettop, BroadBandTestConstants.TEXT_DOUBLE_QUOTE,
			BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandTestConstants.LOG_FILE_PINGTEST,
			BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandTestConstants.SYMBOL_PIPE,
			BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandTestConstants.CMD_TAIL,
			BroadBandTestConstants.STRING_CONSTANT_2);
		status = BroadBandSystemUtils.verifyArmConsoleLog(sshCommand, tapEnv, device, deviceDateTime);
	    }
	    if (status) {
		LOGGER.info("STEP 11 : ACTUAL : The logs related to pingtest results obtained successfully");
	    } else {
		LOGGER.error("STEP 11 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	    /**
	     * Step 12 : Validate TR181 parameter Device.IP.Diagnostics.IPPing.NumberOfRepetitions.
	     */
	    stepNumber = "s12";
	    status = false;
	    errorMessage = "The Number of Repetitions for IPPing not obtained";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 12 : DESCRIPTION : Validate TR181 parameter Device.IP.Diagnostics.IPPing.NumberOfRepetitions");
	    LOGGER.info("STEP 12 : ACTION : EXECUTE THE COMMAND :Device.IP.Diagnostics.IPPing.NumberOfRepetitions");
	    LOGGER.info("STEP 12 : EXPECTED : The Number of Repetitions for IPPing should be returned");
	    LOGGER.info("**********************************************************************************");
	    try {
		status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_NUMBER_OF_REPETITIONS,
			String.valueOf(BroadBandTestConstants.DEVICE_LOCK_SUCCESS));
	    } catch (Exception e) {
		errorMessage = e.getMessage();
		LOGGER.error("Exception occurred while validating NumberOfRepetitions" + errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP 12 : ACTUAL : The Number of Repetitions for IPPing obtained successfully");
	    } else {
		LOGGER.error("STEP 12 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	    /**
	     * Step 13 : Validate TR181 parameter Device.IP.Diagnostics.IPPing.Timeout.
	     */
	    stepNumber = "s13";
	    status = false;
	    errorMessage = "The Timeout for IPPing not obtained";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 13 : DESCRIPTION : Validate TR181 parameter Device.IP.Diagnostics.IPPing.Timeout");
	    LOGGER.info("STEP 13 : ACTION : EXECUTE THE COMMAND :Device.IP.Diagnostics.IPPing.Timeout");
	    LOGGER.info("STEP 13 : EXPECTED : The Timeout for IPPing should be returned");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_TIMEOUT,
		    Long.toString(BroadBandTestConstants.TEN_SECONDS));
	    if (status) {
		LOGGER.info("STEP 13 : ACTUAL : The Timeout for IPPing obtained successfully");
	    } else {
		LOGGER.error("STEP 13 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	    /**
	     * Step 14 : Validate TR181 parameter Device.IP.Diagnostics.IPPing.AverageResponseTime
	     */
	    stepNumber = "s14";
	    status = false;
	    errorMessage = "The Average Response Time not obtained";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 14 : DESCRIPTION : Validate TR181 parameter Device.IP.Diagnostics.IPPing.AverageResponseTime");
	    LOGGER.info(
		    "STEP 14 : ACTION : EXECUTE THE COMMAND : grep -B 1 -i \"AvgRtt\" /rdklogs/logs/TDMlog.txt.0 | tail -2");
	    LOGGER.info("STEP 14 : EXPECTED : The Average Response Time should be returned");
	    LOGGER.info("**********************************************************************************");
	    String averageResponseTime = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_AVERAGE_RESPONSE_TIME);
	    status = (CommonMethods.isNotNull(averageResponseTime) && BroadBandSystemUtils.verifyArmConsoleLog(tapEnv,
		    device, BroadBandTraceConstants.LOG_MESSAGE_AVERAGE_RESPONSE_TIME + averageResponseTime,
		    BroadBandTestConstants.LOG_FILE_PINGTEST, deviceDateTime));
	    if (!status) {
		sshCommand = BroadBandCommonUtils.concatStringUsingStringBuffer(
			BroadBandCommandConstants.CMD_GREP_B_I.replaceAll(
				BroadBandTestConstants.STRING_VALUE_TO_REPLACE,
				BroadBandTestConstants.STRING_CONSTANT_1),
			BroadBandTestConstants.TEXT_DOUBLE_QUOTE,
			BroadBandTraceConstants.LOG_MESSAGE_AVERAGE_RESPONSE_TIME, averageResponseTime,
			BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
			BroadBandTestConstants.LOG_FILE_PINGTEST, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
			BroadBandTestConstants.SYMBOL_PIPE, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
			BroadBandTestConstants.CMD_TAIL, BroadBandTestConstants.STRING_CONSTANT_2);
		status = BroadBandSystemUtils.verifyArmConsoleLog(sshCommand, tapEnv, device, deviceDateTime);
	    }
	    if (status) {
		LOGGER.info("STEP 14 : ACTUAL : The Average Response Time obtained successfully");
	    } else {
		LOGGER.error("STEP 14 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	    /**
	     * Step 15 : Validate if invalid url/host address is successfully set for TR181 param
	     * Device.IP.Diagnostics.IPPing.Host
	     */
	    stepNumber = "s15";
	    status = false;
	    errorMessage = "Unable to set TR181 param Device.IP.Diagnostics.IPPing.Host with invalid hots address/url";
	    try {
		invalidHostAddress = AutomaticsTapApi
			.getSTBPropsValue(BroadBandTestConstants.PROP_KEY_INVALID_HOST_ADDRESS_FOR_PINGTEST);
	    } catch (Exception e) {
		LOGGER.error("Key not found on stb.properties : "
			+ BroadBandTestConstants.PROP_KEY_INVALID_HOST_ADDRESS_FOR_PINGTEST);
		invalidHostAddress = BroadBandTestConstants.PROP_KEY_INVALID_HOST_ADDRESS_FOR_PINGTEST;
	    }
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 15 : DESCRIPTION : Validate if invalid url/host address is successfully set for TR181 param Device.IP.Diagnostics.IPPing.Host");
	    LOGGER.info("STEP 15 : ACTION : EXECUTE THE COMMAND : Device.IP.Diagnostics.IPPing.Host");
	    LOGGER.info(
		    "STEP 15 : EXPECTED : TR181 param Device.IP.Diagnostics.IPPing.Host value should be set successfully");
	    LOGGER.info("**********************************************************************************");
	    /**
	     * The default invalid url configured in "stb.properties" for "pingtest.invalid.host.address" is :
	     * ww.invalidurl.c To ping other urls, please update the value of "pingtest.invalid.host.address" in
	     * "stb.properties"
	     */
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_SET_HOST_ADDRESS, BroadBandTestConstants.CONSTANT_0,
		    invalidHostAddress);
	    if (status) {
		LOGGER.info(
			"STEP 15 : ACTUAL : TR181 param Device.IP.Diagnostics.IPPing.Host was successfully set with Invalid host address");
	    } else {
		LOGGER.error("STEP 15 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	    /**
	     * Step 16 : Trigger ping test by passing invalid url or host address and Validate TR181 parameter
	     * Device.IP.Diagnostics.IPPing.DiagnosticsState.
	     */
	    stepNumber = "s16";
	    status = false;
	    errorMessage = "Invalid url/host address ping test did not return DiagnosticsState value as 'Error_CannotResolveHostName'";
	    deviceDateTime = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 16 : DESCRIPTION : Trigger ping test by passing invalid url or host address and Validate TR181 parameter Device.IP.Diagnostics.IPPing.DiagnosticsState");
	    LOGGER.info("STEP 16 : ACTION : EXECUTE THE COMMAND : Device.IP.Diagnostics.X_RDKCENTRAL-COM_PingTest.Run");
	    LOGGER.info(
		    "STEP 16 : EXPECTED : Ping test should be triggered successfully and TR181 parameter Device.IP.Diagnostics.IPPing.DiagnosticsState should return 'Error_CannotResolveHostName'");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_TRIGGER_PINGTEST, BroadBandTestConstants.CONSTANT_3,
		    BroadBandTestConstants.TRUE);
	    if (status) {
		status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_DIAGNOSTICS_STATE,
			BroadBandTraceConstants.LOG_MESSAGE_INVALID_HOST_NAME);
	    }
	    if (status) {
		LOGGER.info(
			"STEP 16 : ACTUAL : Invalid url/host address ping test returned DiagnosticsState value as 'Error_CannotResolveHostName' as expected");
	    } else {
		LOGGER.error("STEP 16 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

	    /**
	     * Step 17 : Validating TDMlog.txt.0 log file for invalid url/host address ping test.
	     */
	    stepNumber = "s17";
	    status = false;
	    errorMessage = "TDMlog.txt.0 log file doesn't have log entry as 'SuccessCount:0' for invalid url ping test";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 17: Validating TDMlog.txt.0 log file for invalid url/host address ping test");
	    LOGGER.info(
		    "STEP 17 : ACTION : EXECUTE THE COMMAND : grep -B 1 -i \"SuccessCount\" /rdklogs/logs/TDMlog.txt.0 | tail -2");
	    LOGGER.info("STEP 17 : EXPECTED : TDMlog.txt.0 log file should have log entry with 'SuccessCount:0'");
	    LOGGER.info("**********************************************************************************");
	    sshCommand = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.TEXT_DOUBLE_QUOTE,
		    BroadBandTraceConstants.LOG_MESSAGE_SUCCESS_COUNT, BroadBandTestConstants.STRING_ZERO,
		    BroadBandTestConstants.TEXT_DOUBLE_QUOTE);
	    status = BroadBandSystemUtils.verifyArmConsoleLog(tapEnv, device, sshCommand,
		    BroadBandTestConstants.LOG_FILE_PINGTEST, deviceDateTime);
	    if (!status) {
		sshCommand = BroadBandCommonUtils.concatStringUsingStringBuffer(
			BroadBandCommandConstants.CMD_GREP_B_I.replaceAll(
				BroadBandTestConstants.STRING_VALUE_TO_REPLACE,
				BroadBandTestConstants.STRING_CONSTANT_1),
			BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandTraceConstants.LOG_MESSAGE_SUCCESS_COUNT,
			BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
			BroadBandTestConstants.LOG_FILE_PINGTEST, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
			BroadBandTestConstants.SYMBOL_PIPE, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
			BroadBandTestConstants.CMD_TAIL, BroadBandTestConstants.STRING_CONSTANT_2);
		status = BroadBandSystemUtils.verifyArmConsoleLog(sshCommand, tapEnv, device, deviceDateTime);
	    }
	    if (status) {
		LOGGER.info(
			"STEP 17 : ACTUAL : 'SuccessCount:0' log entry found in TDMlog.txt.0 log file for invalid url ping test");
	    } else {
		LOGGER.error("STEP 17 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
	} catch (TestException exception) {
	    LOGGER.error(
		    "TC-RDKB-TELEMETRY-1500 : Execution error occured due to exception --> " + exception.getMessage());
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
		    true);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-TELEMETRY-1500");
    }

}
