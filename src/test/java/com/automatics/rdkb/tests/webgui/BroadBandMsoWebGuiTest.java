/**
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

package com.automatics.rdkb.tests.webgui;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants.BAND_STEERING_PARAM;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants.RdkBBandSteeringParameters;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;
import com.automatics.rdkb.constants.WebPaParamConstants;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.utils.BroadBandBandSteeringUtils;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandMeshUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpMib;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.rdkb.webui.BroadBandWebUiBaseTest;
import com.automatics.snmp.SnmpDataType;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.utils.CommonMethods;
import com.automatics.webpa.WebPaParameter;
import com.automatics.webpa.WebPaServerResponse;

/**
 * Test class with test case for validating web ui based tests
 * 
 * @Refactor Alan_Bivera
 */
public class BroadBandMsoWebGuiTest extends BroadBandWebUiBaseTest {

    /**
     * Test to Validate the default value of band steering Capacity using TR181 data object via WEBPA and verify this
     * Parameter is read only.
     * 
     * <ol>
     * <li>STEP 1:Get the default value of Band Steering capability</li>
     * <li>STEP 2:Set the Band Steering capability as false</li>
     * </ol>
     * 
     * @param device
     * @author anandam.s
     * @Refactor Alan_Bivera
     */
    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
	    BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-WIFI-BAND-STEERING-1001", testDecription = "Test to Validate the default value of band steering Capacity using TR181 data object via WEBPA and verify this paraeter is read only")
    public void testVerifyBandSteeringCapability(Dut device) {
	// Test case id
	String testId = "TC-RDKB-WIFI-BAND-STEERING-101";
	// Test step number
	String testStepNumber = "s1";
	// result variable
	boolean status = false;
	// String to store the error message
	String errorMessage = null;
	// bandsteering status
	boolean defaultBandSteering = true;

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: " + testId);
	LOGGER.info(
		"TEST DESCRIPTION: Validate the default value of band steering Capacity using TR181 data object via WEBPA and verify this paraeter is read only");
	LOGGER.info("*************************************************************************");

	LOGGER.info("************************************************************************************************");
	LOGGER.info("STEP 1:Get the default value of Band Steering capability  ");
	LOGGER.info("EXPECTED: Capability must always report  true  as value. ");
	LOGGER.info("STEP 2:Set the  Band Steering capability  as false");
	LOGGER.info(
		"EXPECTED: Device.WiFi.X_RDKCENTRAL-COM_BandSteering.Capability is a read only webpa and so the reponse should have \"Parameter is not writable\"");

	LOGGER.info("******************************************************");
	LOGGER.info("STEP 1: DESCRIPTION : Get the default value of Band Steering capability");
	LOGGER.info("STEP 1: ACTION      : Execute webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.Capability ");
	LOGGER.info("STEP 1: EXPECTED    : Capability must always report  true  as value.  ");
	LOGGER.info("******************************************************");
	testStepNumber = "s1";
	String bandSteeringCapabilityValue = null;
	errorMessage = "Failed to get the default value of webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.Capability";
	try {
	    bandSteeringCapabilityValue = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_BAND_STEERING_CAPABILITY);
	    LOGGER.info("Webpa response for bandSteeringCapabilityValue  :  " + bandSteeringCapabilityValue);
	    status = Boolean.valueOf(bandSteeringCapabilityValue);
	} catch (TestException e) {
	    errorMessage = e.getMessage();
	    LOGGER.error(errorMessage);
	}

	LOGGER.info("STEP 1: ACTUAL: "
		+ (status
			? "Default value of webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.Capability is "
				+ defaultBandSteering
			: errorMessage));
	tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	LOGGER.info("******************************************************");
	LOGGER.info("STEP 2: DESCRIPTION : Set the  Band Steering capability  as false");
	LOGGER.info("STEP 2: ACTION      : set webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.Capability  as false ");
	LOGGER.info(
		"STEP 2: EXPECTED    :Device.WiFi.X_RDKCENTRAL-COM_BandSteering.Capability is a read only webpa and so the reponse should have \"Parameter is not writable\" ");
	LOGGER.info("******************************************************");
	testStepNumber = "s2";
	errorMessage = "Failed to verify  webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.Capability is read only ";
	LOGGER.info("Verifying  webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.Capability is read only");
	try {
	    WebPaServerResponse serverResponse = BroadBandWebPaUtils.setWebPaParamAndReturnResp(tapEnv, device,
		    BroadBandWebPaConstants.WEBPA_PARAM_BAND_STEERING_CAPABILITY, BroadBandTestConstants.FALSE,
		    BroadBandTestConstants.CONSTANT_3);
	    BroadBandResultObject result = BroadBandWebPaUtils
		    .verifyReadOnlyAtributeOfWebPaParamFromWebPaServerResponse(serverResponse,
			    BroadBandWebPaConstants.WEBPA_PARAM_BAND_STEERING_CAPABILITY);
	    status = result.isStatus();
	    errorMessage = result.getErrorMessage();
	    LOGGER.info("STEP 2: ACTUAL: "
		    + (status
			    ? BroadBandWebPaConstants.WEBPA_PARAM_BAND_STEERING_CAPABILITY
				    + " cannot be writable using WebPA, READ-ONLY attribute verified"
			    : errorMessage));
	} catch (TestException e) {
	    LOGGER.error(e.getMessage());
	}
	tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	LOGGER.debug("ENDING TESTCASE :testVerifyBandSteeringCapability() ");
    }
    
    /**
     * Verify 2.4 GHz WiFi beacon rate changing behavior.
     * <ol>
     * <li>Set the operating mode to b/g/n using WebPA request</li>
     * <li>Verify Beacon rate is set to 1Mbps using WebPA request</li>
     * <li>Verify logging when Beacon rate is changed to 1Mbps with reason</li>
     * <li>Set the operating mode to g/n using WebPA request</li>
     * <li>Verify Beacon rate is set to 6Mbps using WebPA request</li>
     * <li>Verify logging when Beacon rate is changed to 6Mbps with reason</li>
     * <li>Change value of beacon rate to 2Mbps using WebPA request</li>
     * <li>Verify logging when Beacon rate is changed to 2Mbps with reason</li>
     * <li>Reboot the device</li>
     * <li>Verify beacon rate value persists on reboot</li>
     * <li>Set the operating mode to b/g/n using WebPA request</li>
     * <li>Verify Beacon rate is set to 1Mbps using WebPA request</li>
     * <li>Change value of beacon rate to 2Mbps using WebPA request</li>
     * <li>Set the operating mode to g/n using WebPA request</li>
     * <li>Verify Beacon rate is set to 6Mbps using WebPA request</li>
     * </ol>
     * 
     * @author Ashwin sankara
     * @refactor Govardhan
     * 
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WIFI-5001")
    public void verifyAutomaticBeaconRateTest(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-WIFI-501";
	String stepNum = "s1";
	String response = null;
	String errorMessage = null;
	boolean status = false;
	// Variable Declation Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-5001");
	LOGGER.info("TEST DESCRIPTION: Verify 2.4 GHz WiFi beacon rate changing behavior.");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Set the operating mode to b/g/n using WebPA request");
	LOGGER.info("2. Verify Beacon rate is set to 1Mbps using WebPA request");
	LOGGER.info("3. Verify logging when Beacon rate is changed to 1Mbps with reason");
	LOGGER.info("4. Set the operating mode to g/n using WebPA request");
	LOGGER.info("5. Verify Beacon rate is set to 6Mbps using WebPA request");
	LOGGER.info("6. Verify logging when Beacon rate is changed to 6Mbps with reason");
	LOGGER.info("7. Change value of beacon rate to 12Mbps using WebPA request");
	LOGGER.info("8. Verify logging when Beacon rate is changed to 12Mbps with reason");
	LOGGER.info("9. Reboot the device");
	LOGGER.info("10. Verify beacon rate value persists on reboot");
	LOGGER.info("11. Set the operating mode to b/g/n using WebPA request");
	LOGGER.info("12. Verify Beacon rate is set to 1Mbps using WebPA request");
	LOGGER.info("13. Change value of beacon rate to 2Mbps using WebPA request");
	LOGGER.info("14. Set the operating mode to g/n using WebPA request");
	LOGGER.info("15. Verify Beacon rate is set to 6Mbps using WebPA request");

	LOGGER.info("#######################################################################################");

	try {

	    errorMessage = "Failed to change 2.4Ghz operating mode to b,g,n";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Set the operating mode to b/g/n using WebPA request");
	    LOGGER.info("STEP 1: ACTION : Execute WebPA command to set 2.4Ghz Operating mode to b,g,n");
	    LOGGER.info("STEP 1: EXPECTED : Operating mode set to b,g,n successfully");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.verifyWebPaValueAfterDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_10000_OPERATINGSTANDARDS,
		    BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.OPERATING_MODE_BGN,
		    BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);

	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Operating mode set to b,g,n successfully");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s2";
	    errorMessage = "Failed to verify value of 2.4Ghz Beacon rate as 1Mbps";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Verify Beacon rate is set to 1Mbps using WebPA request");
	    LOGGER.info("STEP 2: ACTION : Execute WebPA command to get 2.4Ghz Beacon rate value");
	    LOGGER.info("STEP 2: EXPECTED : Value of 2.4Ghz Beacon rate is updated to 1Mbps");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_10001_X_RDKCENTRAL_COM_BEACONRATE,
		    BroadBandTestConstants.TEXT_ONE_MBPS, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);

	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Value of 2.4Ghz Beacon rate is updated to 1Mbps");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s3";
	    errorMessage = "Unable to find log message when beacon rate changed due to operating mode change to b,g,n";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Verify logging when Beacon rate is changed to 1Mbps with reason");
	    LOGGER.info(
		    "STEP 3: ACTION : Execute command (on atom for AtomSync Devices): grep \"BEACON RATE CHANGED vAP0 6Mbps to 1Mbps by TR-181 Object Device.WiFi.Radio.1.OperatingStandards\" /rdklogs/logs/WiFilog.txt.0");
	    LOGGER.info("STEP 3: EXPECTED : Log message is present with reason when beacon rate is changed to 1Mbps");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device,
		    tapEnv, BroadBandTraceConstants.LOG_MESSAGE_BEACON_CHANGE_MODE_BGN,
		    BroadBandTestConstants.LOCATION_WIFI_LOG, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS));

	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : Log message is present with reason when beacon rate is changed to 1Mbps");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s4";
	    errorMessage = "Failed to change 2.4Ghz operating mode to g,n";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : Set the operating mode to g/n using WebPA request");
	    LOGGER.info("STEP 4: ACTION : Execute WebPA command to set 2.4Ghz Operating mode to g,n");
	    LOGGER.info("STEP 4: EXPECTED : Operating mode set to g,n successfully");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.verifyWebPaValueAfterDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_10000_OPERATINGSTANDARDS,
		    BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.OPERATING_MODE_GN,
		    BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);

	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Operating mode set to g,n successfully");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s5";
	    errorMessage = "Failed to verify value of 2.4Ghz Beacon rate as 6Mbps";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION : Verify Beacon rate is set to 6Mbps using WebPA request");
	    LOGGER.info("STEP 5: ACTION : Execute WebPA command to get 2.4Ghz Beacon rate value");
	    LOGGER.info("STEP 5: EXPECTED : Value of 2.4Ghz Beacon rate is updated to 6Mbps");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_10001_X_RDKCENTRAL_COM_BEACONRATE,
		    BroadBandTestConstants.TEXT_SIX_MBPS, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);

	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : Value of 2.4Ghz Beacon rate is updated to 6Mbps");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s6";
	    errorMessage = "Unable to find log message when beacon rate changed due to operating mode change to g,n";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Verify logging when Beacon rate is changed to 6Mbps with reason");
	    LOGGER.info(
		    "STEP 6: ACTION : Execute command (on atom for AtomSync Devices): grep \"BEACON RATE CHANGED vAP0 1Mbps to 6Mbps by TR-181 Object Device.WiFi.Radio.1.OperatingStandards\" /rdklogs/logs/WiFilog.txt.0");
	    LOGGER.info("STEP 6: EXPECTED : Log message is present with reason when beacon rate is changed to 6Mbps");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device,
		    tapEnv, BroadBandTraceConstants.LOG_MESSAGE_BEACON_CHANGE_MODE_GN,
		    BroadBandTestConstants.LOCATION_WIFI_LOG, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS));

	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : Log message is present with reason when beacon rate is changed to 6Mbps");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s7";
	    errorMessage = "Failed to set 2.4Ghz beacon rate to 12Mbps using webpa";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Change value of beacon rate to 12Mbps using WebPA request");
	    LOGGER.info("STEP 7: ACTION : Execute WebPA command to set 2.4Ghz Beacon rate to 12Mbps");
	    LOGGER.info("STEP 7: EXPECTED : Beacon rate set to 12Mbps successfully");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_10001_X_RDKCENTRAL_COM_BEACONRATE,
		    BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.TEXT_TWELEVE_MBPS);

	    if (status) {
		LOGGER.info("STEP 7: ACTUAL : Beacon rate set to 12Mbps successfully");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s8";
	    errorMessage = "Unable to find log message when beacon rate is changed using webpa";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION : Verify logging when Beacon rate is changed to 12Mbps with reason");
	    LOGGER.info(
		    "STEP 8: ACTION : Execute command (on atom for AtomSync Devices): grep \"BEACON RATE CHANGED vAP0 6Mbps to 12Mbps by TR-181 Object Device.WiFi.AccessPoint.1.X_RDKCENTRAL-COM_BeaconRate\" /rdklogs/logs/WiFilog.txt.0");
	    LOGGER.info("STEP 8: EXPECTED : Log message is present with reason when beacon rate is changed to 12Mbps");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device,
		    tapEnv, BroadBandTraceConstants.LOG_MESSAGE_BEACON_PARAM_CHANGE,
		    BroadBandTestConstants.LOCATION_WIFI_LOG, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS));

	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : Log message is present with reason when beacon rate is changed to 12Mbps");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s9";
	    errorMessage = "Failed to reboot the device successfully";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 9: DESCRIPTION : Reboot the device");
	    LOGGER.info("STEP 9: ACTION : Execute command: /sbin/reboot");
	    LOGGER.info("STEP 9: EXPECTED : Device rebooted successfully");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);

	    if (status) {
		LOGGER.info("STEP 9: ACTUAL : Device rebooted successfully");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s10";
	    errorMessage = "Failed to obtain value of 2.4Ghz Beacon rate";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 10: DESCRIPTION : Verify beacon rate value persists on reboot");
	    LOGGER.info("STEP 10: ACTION : Execute WebPA command to get 2.4Ghz Beacon rate value");
	    LOGGER.info("STEP 10: EXPECTED : Value of 2.4Ghz Beacon rate persists after reboot");
	    LOGGER.info("**********************************************************************************");

	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_10001_X_RDKCENTRAL_COM_BEACONRATE);
	    if (CommonMethods.isNotNull(response)) {
		errorMessage = "Value of 2.4Ghz beacon rate did not persist after reboot, expected: 12Mbps actual: "
			+ response;
		status = response.equalsIgnoreCase(BroadBandTestConstants.TEXT_TWELEVE_MBPS);
	    }

	    if (status) {
		LOGGER.info("STEP 10: ACTUAL : Value of 2.4Ghz Beacon rate persists after reboot");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s11";
	    errorMessage = "Failed to change 2.4Ghz operating mode to b,g,n";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 11: DESCRIPTION : Set the operating mode to b/g/n using WebPA request");
	    LOGGER.info("STEP 11: ACTION : Execute WebPA command to set 2.4Ghz Operating mode to b,g,n");
	    LOGGER.info("STEP 11: EXPECTED : Operating mode set to b,g,n successfully");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.verifyWebPaValueAfterDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_10000_OPERATINGSTANDARDS,
		    BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.OPERATING_MODE_BGN,
		    BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);

	    if (status) {
		LOGGER.info("STEP 11: ACTUAL : Operating mode set to b,g,n successfully");
	    } else {
		LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s12";
	    errorMessage = "Failed to verify value of 2.4Ghz Beacon rate as 1Mbps";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 12: DESCRIPTION : Verify Beacon rate is set to 1Mbps using WebPA request");
	    LOGGER.info("STEP 12: ACTION : Execute WebPA command to get 2.4Ghz Beacon rate value");
	    LOGGER.info("STEP 12: EXPECTED : Value of 2.4Ghz Beacon rate is updated to 1Mbps");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_10001_X_RDKCENTRAL_COM_BEACONRATE,
		    BroadBandTestConstants.TEXT_ONE_MBPS, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);

	    if (status) {
		LOGGER.info("STEP 12: ACTUAL : Value of 2.4Ghz Beacon rate is updated to 1Mbps");
	    } else {
		LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s13";
	    errorMessage = "Failed to set 2.4Ghz beacon rate to 2Mbps using webpa";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 13: DESCRIPTION : Change value of beacon rate to 2Mbps using WebPA request");
	    LOGGER.info("STEP 13: ACTION : Execute WebPA command to set 2.4Ghz Beacon rate to 2Mbps");
	    LOGGER.info("STEP 13: EXPECTED : Beacon rate set to 2Mbps successfully");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_10001_X_RDKCENTRAL_COM_BEACONRATE,
		    BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.TEXT_TWO_MBPS);

	    if (status) {
		LOGGER.info("STEP 13: ACTUAL : Beacon rate set to 2Mbps successfully");
	    } else {
		LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s14";
	    errorMessage = "Failed to change 2.4Ghz operating mode to g,n";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 14: DESCRIPTION : Set the operating mode to g/n using WebPA request");
	    LOGGER.info("STEP 14: ACTION : Execute WebPA command to set 2.4Ghz Operating mode to g,n");
	    LOGGER.info("STEP 14: EXPECTED : Operating mode set to g,n successfully");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.verifyWebPaValueAfterDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_10000_OPERATINGSTANDARDS,
		    BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.OPERATING_MODE_GN,
		    BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);

	    if (status) {
		LOGGER.info("STEP 14: ACTUAL : Operating mode set to g,n successfully");
	    } else {
		LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s15";
	    errorMessage = "Failed to verify value of 2.4Ghz Beacon rate as 6Mbps";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 15: DESCRIPTION : Verify Beacon rate is set to 6Mbps using WebPA request");
	    LOGGER.info("STEP 15: ACTION : Execute WebPA command to get 2.4Ghz Beacon rate value");
	    LOGGER.info("STEP 15: EXPECTED : Value of 2.4Ghz Beacon rate is updated to 6Mbps");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_10001_X_RDKCENTRAL_COM_BEACONRATE,
		    BroadBandTestConstants.TEXT_SIX_MBPS, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);

	    if (status) {
		LOGGER.info("STEP 15: ACTUAL : Value of 2.4Ghz Beacon rate is updated to 6Mbps");
	    } else {
		LOGGER.error("STEP 15: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	} finally {

	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    LOGGER.info(
		    "POST-CONDITION : DESCRIPTION : Verify else reset default operating mode as g,n and beacon rate as 6Mbps");
	    LOGGER.info(
		    "POST-CONDITION : ACTION : Execute WebPA requests to set operating mode and beacon rate with default values");
	    LOGGER.info("POST-CONDITION : EXPECTED : Post condition executed successfully");

	    status = false;
	    errorMessage = "Failed to reset default operating mode as g,n";
	    if (BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_10000_OPERATINGSTANDARDS,
		    BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.OPERATING_MODE_GN,
		    BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS)) {
		errorMessage = "Failed to reset default beacon rate as 6Mbps";
		status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_10001_X_RDKCENTRAL_COM_BEACONRATE,
			BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.TEXT_SIX_MBPS,
			BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
	    }

	    if (status) {
		LOGGER.info("POST-CONDITION : ACTUAL : Post condition executed successfully");
	    } else {
		LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed: " + errorMessage);
	    }
	    LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-5001");
    }
    
    /**
     * This Test verify the factory default value of Band steering parameters using webpa and dmcli
     * <ol>
     * <li>STEP 1 : Perform a factory reset of the DUT using webpa</li>
     * <li>STEP 2 : Get the webpa parameter Device.WiFi.X_RDKCENTRAL-COM_BandSteering.Enable</li>
     * <li>STEP 3 : Verify the factory default value of IdleInactiveTime for 2.4 ghz radio using webpa</li>
     * <li>STEP 4 : Verify the factory default value of IdleInactiveTime for 5 ghz radio using webpa</li>
     * <li>STEP 5 : Verify the factory default value of OverloadInactiveTime for 2.4 ghz radio using webpa</li>
     * <li>STEP 6 : Verify the factory default value of OverloadInactiveTime for 5 ghz radio using webpa</li>
     * <li>STEP 7 : Verify the factory default value of APGroup using webpa</li>
     * </ol>
     * 
     * @param device
     * @author anandam.s
     * @refactor Alan_Bivera
     */
    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
	    BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-WIFI-BAND-STEERING-1003")
    public void testVerifyFactoryResetValuesForBandSteering(Dut device) {

	// Test case id
	String testId = "TC-RDKB-WIFI-BAND-STEERING-103";
	// Test step number
	String testStepNumber = "s1";
	// String to store the error message
	String errorMessage = null;
	// Instance of BroadBandResultObject .Stores status and error messages
	BroadBandResultObject resultObject = new BroadBandResultObject();
	// step status
	boolean status = false;
	/** Map for holding webpa response */
	Map<String, String> bandSteeringWebpaResponse = new HashMap<String, String>();
	// Interger to store pre condition number
	int preCondNumber = 1;

	LOGGER.debug("STARTING TESTCASE :testVerifyFactoryResetValuesForBandSteering() ");

	try {
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: " + testId);
	    LOGGER.info(
		    "TEST DESCRIPTION: verify  the factory default value of  Band steering paarmeters using webpa and dmcli");
	    LOGGER.info("*************************************************************************");
	    LOGGER.info(
		    "************************************************************************************************");
	    LOGGER.info("STEP 1:Perform a factory reset of the DUT using webpa ");
	    LOGGER.info("STEP 2:Get the webpa parameter Device.WiFi.X_RDKCENTRAL-COM_BandSteering.Enable.");
	    LOGGER.info("STEP 3: Verify the factory default value of IdleInactiveTime for 2.4 ghz radio using webpa  ");
	    LOGGER.info("STEP 4: Verify the factory default value of IdleInactiveTime for 5 ghz radio using webpa  ");
	    LOGGER.info(
		    "STEP 5: Verify the factory default value of OverloadInactiveTime for 2.4 ghz radio using webpa  ");
	    LOGGER.info(
		    "STEP 6: Verify the factory default value of OverloadInactiveTime for 5 ghz radio using webpa  ");
	    LOGGER.info("STEP 7: Verify the factory default value of APGroup  using webpa  ");
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("PRE-CONDITION " + preCondNumber + ": DESCRIPTION : Disable Mesh");
	    LOGGER.info("PRE-CONDITION " + preCondNumber + ": ACTION : Execute command to disable Mesh");
	    LOGGER.info("PRE-CONDITION " + preCondNumber + ": EXPECTED : MEsh should be disabled");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Failed to disable WiFi mesh service using WebPA";
	    status = BroadBandMeshUtils.enableOrDisableMesh(device, tapEnv, false);
	    if (status) {
		LOGGER.info("PRE-CONDITION " + preCondNumber + ": ACTUAL : Successfully Disabled Mesh");
	    } else {
		LOGGER.error("PRE-CONDITION " + preCondNumber + ":ACTUAL : " + errorMessage);
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION : FAILED : " + errorMessage);
	    }
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
	    LOGGER.info("******************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Perform a factory reset of the DUT using webpa   ");
	    LOGGER.info("STEP 1: ACTION      : Execute a set on webpa Device.X_CISCO_COM_DeviceControl.FactoryReset ");
	    LOGGER.info("STEP 1: EXPECTED    : WebPA request should return success message . ");
	    LOGGER.info("******************************************************");
	    testStepNumber = "s1";
	    status = false;
	    errorMessage = "Failed to factory reset wifi interface using WebPa parameter "
		    + WebPaParamConstants.WEBPA_PARAM_FACTORY_RESET;
	    status = BroadBandCommonUtils.performFactoryResetWebPa(tapEnv, device);
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Status of Factory reset using webpa is true");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("******************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    LOGGER.info("Wait for 5 mins  for the device to be completely up");
	    tapEnv.waitTill(BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS);

	    LOGGER.info("Reactivating the device using WebPa Parameter");
	    BroadBandWiFiUtils.reactivateDeviceUsingWebPa(tapEnv, device);

	    bandSteeringWebpaResponse = tapEnv.executeMultipleWebPaGetCommands(device,
		    RdkBBandSteeringParameters.getAllBandSteeringWebPaConstantNames());

	    LOGGER.info("******************************************************");
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Get the webpa parameter Device.WiFi.X_RDKCENTRAL-COM_BandSteering.Enable ");
	    LOGGER.info(
		    "STEP 2: ACTION      : Execute a get on webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.Enable ");
	    LOGGER.info("STEP 2: EXPECTED    : Curl output confirms that Band steering is enabled by default. ");
	    LOGGER.info("******************************************************");
	    testStepNumber = "s2";
	    status = false;

		errorMessage = "Failed to get the wepa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.Enable .";
		resultObject = BroadBandBandSteeringUtils.getAndCompareWebpaOrDmcliValues(device, tapEnv,
			RdkBBandSteeringParameters.DEFAULT_STEERING_ENABLE.getParam(),
			RdkBBandSteeringParameters.DEFAULT_STEERING_ENABLE.getDefaultValue(), true,
			bandSteeringWebpaResponse);
		errorMessage += resultObject.getErrorMessage();
		if (resultObject.isStatus()) {
		    LOGGER.info(
			    "STEP 2: ACTUAL : Default value for webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.Enable after factory reset   is  true");
		} else {
		    LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
		}
		LOGGER.info("******************************************************************");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, resultObject.isStatus(), errorMessage,
			false);
	    
	    if (DeviceModeHandler.isDSLDevice(device)||DeviceModeHandler.isFibreDevice(device)|| CommonMethods.isAtomSyncAvailable(device, tapEnv)) {

		LOGGER.info("******************************************************");
		LOGGER.info(
			"STEP 3: DESCRIPTION : Verify the factory default value of IdleInactiveTime for 2.4 ghz radio using webpa ");
		LOGGER.info(
			"STEP 3: ACTION      : Execute a webpa on Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.1.IdleInactiveTime ");
		LOGGER.info("STEP 3: EXPECTED    : The factory default value should be 10  ");
		LOGGER.info("******************************************************");
		testStepNumber = "s3";
		status = false;
		errorMessage = "Failed to get the webpa  Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.1.IdleInactiveTime . ";

		resultObject = BroadBandBandSteeringUtils.getAndCompareWebpaOrDmcliValues(device, tapEnv,
				RdkBBandSteeringParameters.DEFAULT_STEERING_IDLE_INACTIVE_TIME_2GHZ.getParam(),
				RdkBBandSteeringParameters.DEFAULT_STEERING_IDLE_INACTIVE_TIME_2GHZ.getDefaultValue(),
				true, bandSteeringWebpaResponse);
		errorMessage += resultObject.getErrorMessage();
		if (resultObject.isStatus()) {
		    LOGGER.info(
			    "STEP 3: ACTUAL : Status of checking  default value for webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.1.IdleInactiveTime after factory reset is true");
		} else {
		    LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
		}

		LOGGER.info("******************************************************************");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, resultObject.isStatus(), errorMessage,
			false);

		LOGGER.info("******************************************************");
		LOGGER.info(
			"STEP 4: DESCRIPTION : Verify the factory default value of IdleInactiveTime for 5 ghz radio using webpa ");
		LOGGER.info(
			"STEP 4: ACTION      : Execute a webpa on Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.2.IdleInactiveTime ");
		LOGGER.info("STEP 4: EXPECTED    : The factory default value should be 10  ");
		LOGGER.info("******************************************************");
		testStepNumber = "s4";
		status = false;
		errorMessage = "Failed to get the wepa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.2.IdleInactiveTime";

		resultObject = BroadBandBandSteeringUtils.getAndCompareWebpaOrDmcliValues(device, tapEnv,
				RdkBBandSteeringParameters.DEFAULT_STEERING_IDLE_INACTIVE_TIME_5GHZ.getParam(),
				RdkBBandSteeringParameters.DEFAULT_STEERING_IDLE_INACTIVE_TIME_5GHZ.getDefaultValue(),
				true, bandSteeringWebpaResponse);
		errorMessage += resultObject.getErrorMessage();
		if (resultObject.isStatus()) {
		    LOGGER.info(
			    "STEP 4: ACTUAL : Status of checking  default value for webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.2.IdleInactiveTime after factory reset is true");
		} else {
		    LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
		}
		LOGGER.info("******************************************************************");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, resultObject.isStatus(), errorMessage,
			false);

		LOGGER.info("******************************************************");
		LOGGER.info(
			"STEP 5: DESCRIPTION : Verify the factory default value of OverloadInactiveTime for 2.4 ghz radio using webpa ");
		LOGGER.info(
			"STEP 5: ACTION      : Execute a webpa on Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.1.OverloadInactiveTime");
		LOGGER.info("STEP 5: EXPECTED    : The factory default value should be 10  ");
		LOGGER.info("******************************************************");
		testStepNumber = "s5";
		status = false;
		errorMessage = "Failed to get the webpa on  Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.1.OverloadInactiveTime";

		resultObject = BroadBandBandSteeringUtils.getAndCompareWebpaOrDmcliValues(device, tapEnv,
				RdkBBandSteeringParameters.DEFAULT_STEERING_OVERLOAD_INACTIVE_TIME_2GHZ.getParam(),
				RdkBBandSteeringParameters.DEFAULT_STEERING_OVERLOAD_INACTIVE_TIME_2GHZ
					.getDefaultValue(),
				true, bandSteeringWebpaResponse);
		errorMessage += resultObject.getErrorMessage();
		if (resultObject.isStatus()) {
		    LOGGER.info(
			    "STEP 5: ACTUAL : Status of checking  default value for webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.1.OverloadInactiveTime after factory reset is true");
		} else {
		    LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
		}
		LOGGER.info("******************************************************************");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, resultObject.isStatus(), errorMessage,
			false);

		LOGGER.info("******************************************************");
		LOGGER.info(
			"STEP 6: DESCRIPTION : Verify the factory default value of OverloadInactiveTime for 5 ghz radio using webpa ");
		LOGGER.info(
			"STEP 6: ACTION      : Execute a webpa on Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.2.OverloadInactiveTime");
		LOGGER.info("STEP 6: EXPECTED    : The factory default value should be 10  ");
		LOGGER.info("******************************************************");
		testStepNumber = "s6";
		status = false;
		errorMessage = "Failed to get the webpa on  Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.2.OverloadInactiveTime";

		resultObject = BroadBandBandSteeringUtils.getAndCompareWebpaOrDmcliValues(device, tapEnv,
				RdkBBandSteeringParameters.DEFAULT_STEERING_OVERLOAD_INACTIVE_TIME_5GHZ.getParam(),
				RdkBBandSteeringParameters.DEFAULT_STEERING_OVERLOAD_INACTIVE_TIME_5GHZ
					.getDefaultValue(),
				true, bandSteeringWebpaResponse);
		errorMessage += resultObject.getErrorMessage();
		if (resultObject.isStatus()) {
		    LOGGER.info(
			    "STEP 6: ACTUAL : Status of checking  default value for webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.2.OverloadInactiveTime after factory reset is true");
		} else {
		    LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
		}
		LOGGER.info("******************************************************************");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, resultObject.isStatus(), errorMessage,
			false);

	    } else {

		for (int loopCounter = BroadBandTestConstants.CONSTANT_3; loopCounter <= BroadBandTestConstants.CONSTANT_6; loopCounter++) {

		    errorMessage = "STEP " + loopCounter
			    + " is not applicable for Quantenna-based devices. Hence updating this step as Not Applicable";
		    LOGGER.info(errorMessage);

		    testStepNumber = "s" + Integer.toString(loopCounter);
		    LOGGER.info("******************************************************************");
		    tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
			    errorMessage, false);
		}
	    }
	    LOGGER.info("******************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Verify the factory default value of APGroup  using webpa  ");
	    LOGGER.info(
		    "STEP 7: ACTION      : Execute : Execute webpa  on Device.WiFi.X_RDKCENTRAL-COM_BandSteering.APGroup");
	    LOGGER.info("STEP 7: EXPECTED    : The factory default value should be 1,2  ");
	    LOGGER.info("******************************************************");
	    testStepNumber = "s7";
	    status = false;
	    errorMessage = "Failed to get the webpa on  Device.WiFi.X_RDKCENTRAL-COM_BandSteering.APGroup";

	    resultObject = BroadBandBandSteeringUtils.getAndCompareWebpaOrDmcliValues(device, tapEnv,
		    RdkBBandSteeringParameters.DEFAULT_STEERING_APGROUP.getParam(),
		    RdkBBandSteeringParameters.DEFAULT_STEERING_APGROUP.getDefaultValue(), true,
		    bandSteeringWebpaResponse);
	    errorMessage += resultObject.getErrorMessage();
	    if (resultObject.isStatus()) {
		LOGGER.info(
			"STEP 7: ACTUAL : Status of checking  default value for webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.APGroup after factory reset is true");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("******************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, resultObject.isStatus(), errorMessage, false);

	} catch (Exception exception) {
	    errorMessage = "Exception occured during execution !!!!" + exception.getMessage();
	    LOGGER.error(errorMessage);
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, false, errorMessage, false);

	} 
	finally {
	    BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
	}
    }
    
    /**
     * 
     * Verify Set or Get PhyRate threshold for 5GHz band to initiate Steering using TR69 data objects
     * 
     * <ol>
     * <li>STEP 1:Get the default band utilization values using webpa
     * Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.2.PhyRateThreshold</li>
     * <li>STEP 2:Execute a CURL COMMAND to change the
     * Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.2.PhyRateThreshold object to desired threshold to be set.
     * </li>
     * <li>STEP 3:Execute a CURL COMMAND onDevice.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.2. PhyRateThreshold
     * Objects and verify the objects for subscriber private Wi-Fi network</li>
     * </ol>
     * 
     * @param device
     * @author anandam.s
     * @reactor Alan_Bivea
     */
    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
	    BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-WIFI-BAND-STEERING-1004")
    public void testVerifySetAndGetPhysicalRateThresholdFor5GhzRadio(Dut device) {
	// Test case id
	String testId = "TC-RDKB-WIFI-BAND-STEERING-104";
	// Test step number
	String testStepNumber = "s1";
	// String to store the error message
	String errorMessage = null;
	// status variable
	boolean status = false;
	// initial value
	String phyThresholdBeforeSetOperation = null;
	// Variable to store response
	String response = null;
	LOGGER.debug("STARTING TESTCASE :testVerifySetAndGetPhysicalRateThresholdFor5GhzRadio() ");

	try {
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: " + testId);
	    LOGGER.info(
		    "TEST DESCRIPTION: Verify Set or Get PhyRate threshold for 5GHz band to initiate Steering using TR69 data objects");
	    LOGGER.info("*************************************************************************");

	    LOGGER.info(
		    "************************************************************************************************");
	    LOGGER.info(
		    "STEP 1:Get the default band utilization values using webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.2.PhyRateThreshold ");
	    LOGGER.info(
		    "EXPECTED: The CURL COMMAND should get executed successfully and the device response should show Subscriber private Wi-Fi network band steering entries should show Default Band Utilization values.  ");
	    LOGGER.info(
		    "STEP 2:Execute a CURL COMMAND to change the Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.2.PhyRateThreshold object to desired threshold to be set.");
	    LOGGER.info(
		    "EXPECTED: The CURL COMMAND should get executed successfully and should not show any failure or Fault Code. ");
	    LOGGER.info(
		    "STEP 3: Execute a CURL COMMAND onDevice.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.2.PhyRateThreshold  Objects and verify the objects for subscriber private Wi-Fi network   ");
	    LOGGER.info(
		    "EXPECTED:The CURL COMMAND should get executed successfully and the device should show Band Utilization values.");

	    LOGGER.info("******************************************************");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Get the default band utilization values using webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.2.PhyRateThreshold   ");
	    LOGGER.info(
		    "STEP 1: ACTION      : Execute a get on webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.2.PhyRateThreshold ");
	    LOGGER.info(
		    "STEP 1: EXPECTED    : The CURL COMMAND should get executed successfully and the device response should show Subscriber private Wi-Fi network band steering entries should show Default Band Utilization values. . ");
	    LOGGER.info("******************************************************");
	    errorMessage = "Failed to get the default band utilization values for 5 ghz using webpa";
	    phyThresholdBeforeSetOperation = BroadBandBandSteeringUtils.getBandSteeringPhyThreshold(device, tapEnv,
		    WiFiFrequencyBand.WIFI_BAND_5_GHZ);
	    LOGGER.info("STEP 1: Default BandUtilization value is  " + phyThresholdBeforeSetOperation);
	    if (CommonUtils.isNotEmptyOrNull(phyThresholdBeforeSetOperation)) {
		if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		    status = phyThresholdBeforeSetOperation
			    .equals(BroadBandTestConstants.WEBPA_DEFAULT_VALUE_BAND_STEERING_PHY_THRESHOLD_5GHZ);
		} else {
		    status = phyThresholdBeforeSetOperation
			    .equals(BroadBandTestConstants.WEBPA_DEFAULT_VALUE_BAND_STEERING_PHY_THRESHOLD_5GHZ);
		    if (!status) {
			response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
				BroadBandTestConstants.TEXT_DOUBLE_QUOTE
					+ BroadBandWebPaConstants.WEBPA_PARAM_BAND_STEERING_PHY_THRESHOLD_5GHZ
					+ BroadBandTestConstants.TEXT_DOUBLE_QUOTE,
				BroadBandCommandConstants.FILE_RFC_CONFIGDATA_LOG,
				BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
				BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
			LOGGER.info("RESPONSE" + response);
			status = CommonMethods.isNotNull(response) && response.contains(phyThresholdBeforeSetOperation);
		    }
		}
	    }
	    if (!status) {
		errorMessage = "Expected default value is "
			+ (CommonMethods.isAtomSyncAvailable(device, tapEnv)
				? BroadBandTestConstants.WEBPA_DEFAULT_VALUE_BAND_STEERING_PHY_THRESHOLD_5GHZ
				: BroadBandTestConstants.STRING_VALUE_50)
			+ " but observed value is " + phyThresholdBeforeSetOperation;
		LOGGER.error("STEP 1: " + errorMessage);
	    }
	    LOGGER.info("STEP 1 : ACTUAL : "
		    + (status ? "Status of getting Default Band Utilization value for 5ghz using webpa  is true "
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    LOGGER.info("******************************************************");
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Execute a CURL COMMAND to change the Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.2.PhyRateThreshold object to desired threshold to be set. ");
	    LOGGER.info(
		    "STEP 2: ACTION      : Execute a set on webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.2.PhyRateThreshold with value 40");
	    LOGGER.info(
		    "STEP 2: EXPECTED    : The CURL COMMAND should get executed successfully and should not show any failure or Fault Code.");
	    LOGGER.info("******************************************************");
	    errorMessage = "Failed to set the band utilization values for 5ghz using webpa with value 40";
	    testStepNumber = "s2";
	    status = false;
	    status = BroadBandBandSteeringUtils.setBandSteeringPhyThreshold(device, tapEnv,
		    WiFiFrequencyBand.WIFI_BAND_5_GHZ, BroadBandTestConstants.BAND_STEERING_PHY_THRESHOLD_VALUE);
	    LOGGER.info("STEP 2 : ACTUAL : " + (status
		    ? "Status of setting the band utilization values for 5ghz using webpa with value 40 is true "
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    LOGGER.info("******************************************************");
	    LOGGER.info(
		    "STEP 3: DESCRIPTION : Get the band utilization values using webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.2.PhyRateThreshold   ");
	    LOGGER.info(
		    "STEP 3: ACTION      : Execute a get on webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.2.PhyRateThreshold ");
	    LOGGER.info(
		    "STEP 3: EXPECTED    : The CURL COMMAND should get executed successfully and the device response should show Band Utilization values. . ");
	    LOGGER.info("******************************************************");
	    testStepNumber = "s3";
	    status = false;
	    errorMessage = "Failed to get the default band utilization values for 5 ghz using webpa";
	    String phyThresholdAfterSetOperation = BroadBandBandSteeringUtils.getBandSteeringPhyThreshold(device,
		    tapEnv, WiFiFrequencyBand.WIFI_BAND_5_GHZ);
	    LOGGER.info(" BandUtilization value retrieved for 5ghz after set operation is   "
		    + phyThresholdAfterSetOperation);
	    status = CommonUtils.isNotEmptyOrNull(phyThresholdAfterSetOperation)
		    && (phyThresholdAfterSetOperation.equals(BroadBandTestConstants.BAND_STEERING_PHY_THRESHOLD_VALUE));
	    LOGGER.info("STEP 3 : ACTUAL : "
		    + (status ? "Status of getting Default Band Utilization value for 5ghz using webpa  is true "
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	} catch (Exception exception) {
	    errorMessage = "Exception occured during execution !!!!" + exception.getMessage();
	    LOGGER.error(errorMessage);
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, false, errorMessage, false);

	} finally {
	    BroadBandBandSteeringUtils.setBandSteeringPhyThreshold(device, tapEnv, WiFiFrequencyBand.WIFI_BAND_5_GHZ,
		    phyThresholdBeforeSetOperation);
	    LOGGER.info("Parameter reverted to default value ");
	}

	LOGGER.debug("ENDING TESTCASE :testVerifySetAndGetPhysicalRateThresholdFor5GhzRadio() ");

    }

    /**
     * 
     * Verify Set or Get PhyRate threshold for 2.4GHz band to initiate Steering using TR69 data objects
     * 
     * <ol>
     * <li>STEP 1:Get the default band utilization values using webpa
     * Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.1.PhyRateThreshold</li>
     * <li>STEP 2:Execute a CURL COMMAND to change the
     * Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.1.PhyRateThreshold object to desired threshold to be set.
     * </li>
     * <li>STEP 3:Execute a CURL COMMAND onDevice.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.1. PhyRateThreshold
     * Objects and verify the objects for subscriber private Wi-Fi network</li>
     * </ol>
     * 
     * @param device
     * @author anandam.s
     * @reactor Alan_Bivea
     */
    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
	    BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-WIFI-BAND-STEERING-1005")
    public void testVerifySetAndGetPhysicalRateThresholdFor2GHzRadio(Dut device) {
	// Test case id
	String testId = "TC-RDKB-WIFI-BAND-STEERING-105";
	// Test step number
	String testStepNumber = "s1";
	// String to store the error message
	String errorMessage = null;
	boolean status = false;
	// initial value
	String phyThresholdBeforeSetOperation = null;

	// Variable to store response
	String response = null;
	LOGGER.debug("STARTING TESTCASE :testVerifySetAndGetPhysicalRateThresholdFor2GHzRadio() ");

	try {
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: " + testId);
	    LOGGER.info(
		    "TEST DESCRIPTION: Verify Set or Get PhyRate threshold for 2.4GHz band to initiate Steering using TR69 data objects");
	    LOGGER.info("*************************************************************************");

	    LOGGER.info(
		    "************************************************************************************************");
	    LOGGER.info(
		    "STEP 1:Get the default band utilization values using webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.1.PhyRateThreshold ");
	    LOGGER.info(
		    "EXPECTED: The CURL COMMAND should get executed successfully and the device response should show Subscriber private Wi-Fi network band steering entries should show Default Band Utilization values.  ");
	    LOGGER.info(
		    "STEP 2:Execute a CURL COMMAND to change the Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.1.PhyRateThreshold object to desired threshold to be set.");
	    LOGGER.info(
		    "EXPECTED: The CURL COMMAND should get executed successfully and should not show any failure or Fault Code. ");
	    LOGGER.info(
		    "STEP 3: Execute a CURL COMMAND onDevice.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.1.PhyRateThreshold  Objects and verify the objects for subscriber private Wi-Fi network   ");
	    LOGGER.info(
		    "EXPECTED:The CURL COMMAND should get executed successfully and the device should show Band Utilization values.");

	    LOGGER.info("******************************************************");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Get the default band utilization values using webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.1.PhyRateThreshold   ");
	    LOGGER.info(
		    "STEP 1: ACTION      : Execute a get on webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.1.PhyRateThreshold ");
	    LOGGER.info(
		    "STEP 1: EXPECTED    : The CURL COMMAND should get executed successfully and the device response should show Subscriber private Wi-Fi network band steering entries should show Default Band Utilization values. . ");
	    LOGGER.info("******************************************************");
	    errorMessage = "Failed to get the default band utilization values for 2.4 ghz using webpa";
	    phyThresholdBeforeSetOperation = BroadBandBandSteeringUtils.getBandSteeringPhyThreshold(device, tapEnv,
		    WiFiFrequencyBand.WIFI_BAND_2_GHZ);
	    LOGGER.info("STEP 1: Default BandUtilization value is  " + phyThresholdBeforeSetOperation);
	    if (CommonUtils.isNotEmptyOrNull(phyThresholdBeforeSetOperation)) {
		if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		    status = phyThresholdBeforeSetOperation
			    .equals(BroadBandTestConstants.WEBPA_DEFAULT_VALUE_BAND_STEERING_PHY_THRESHOLD_2GHZ);
		} else {
		    status = phyThresholdBeforeSetOperation
			    .equals(BroadBandTestConstants.WEBPA_DEFAULT_VALUE_BAND_STEERING_PHY_THRESHOLD_2GHZ);
		    if (!status) {
			response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
				BroadBandTestConstants.TEXT_DOUBLE_QUOTE
					+ BroadBandWebPaConstants.WEBPA_PARAM_BAND_STEERING_PHY_THRESHOLD_2_4GHZ
					+ BroadBandTestConstants.TEXT_DOUBLE_QUOTE,
				BroadBandCommandConstants.FILE_RFC_CONFIGDATA_LOG,
				BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
				BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
			LOGGER.info("RESPONSE" + response);
			status = CommonMethods.isNotNull(response) && response.contains(phyThresholdBeforeSetOperation);
		    }
		}
	    }
	    if (!status) {
		errorMessage = "Expected default value is "
			+ (CommonMethods.isAtomSyncAvailable(device, tapEnv)
				? BroadBandTestConstants.WEBPA_DEFAULT_VALUE_BAND_STEERING_PHY_THRESHOLD_2GHZ
				: BroadBandTestConstants.STRING_30)
			+ " but observed value is " + phyThresholdBeforeSetOperation;
		LOGGER.error("STEP 1: " + errorMessage);
	    }
	    LOGGER.info("STEP 1 : ACTUAL :"
		    + (status ? " Status of getting Default Band Utilization value for 2.4ghz using webpa is true  "
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    LOGGER.info("******************************************************");
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Execute a CURL COMMAND to change the Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.1.PhyRateThreshold object to desired threshold to be set. ");
	    LOGGER.info(
		    "STEP 2: ACTION      : Execute a set on webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.1.PhyRateThreshold with value 40");
	    LOGGER.info(
		    "STEP 2: EXPECTED    : The CURL COMMAND should get executed successfully and should not show any failure or Fault Code.");
	    LOGGER.info("******************************************************");
	    errorMessage = "Failed to set the band utilization values for 2.4ghz using webpa with value 40";
	    testStepNumber = "s2";
	    status = false;
	    status = BroadBandBandSteeringUtils.setBandSteeringPhyThreshold(device, tapEnv,
		    WiFiFrequencyBand.WIFI_BAND_2_GHZ, BroadBandTestConstants.BAND_STEERING_PHY_THRESHOLD_VALUE);
	    LOGGER.info("STEP 2 : ACTUAL :" + (status
		    ? " Status of setting the band utilization values for 2.4ghz using webpa with value 40 is true "
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    LOGGER.info("******************************************************");
	    LOGGER.info(
		    "STEP 3: DESCRIPTION : Get the band utilization values using webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.1.PhyRateThreshold   ");
	    LOGGER.info(
		    "STEP 3: ACTION      : Execute a get on webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.1.PhyRateThreshold ");
	    LOGGER.info(
		    "STEP 3: EXPECTED    : The CURL COMMAND should get executed successfully and the device response should show Band Utilization values. . ");
	    LOGGER.info("******************************************************");
	    testStepNumber = "s3";
	    status = false;
	    errorMessage = "Failed to get the default band utilization values for 2.4 ghz using webpa";
	    String phyThresholdAfterSetOperation = BroadBandBandSteeringUtils.getBandSteeringPhyThreshold(device,
		    tapEnv, WiFiFrequencyBand.WIFI_BAND_2_GHZ);
	    LOGGER.info(" BandUtilization value retrieved for 2.4ghz after set operation is   "
		    + phyThresholdAfterSetOperation);
	    status = CommonUtils.isNotEmptyOrNull(phyThresholdAfterSetOperation)
		    && (phyThresholdAfterSetOperation.equals(BroadBandTestConstants.BAND_STEERING_PHY_THRESHOLD_VALUE));
	    LOGGER.info("STEP 3 : ACTUAL :"
		    + (status ? " Status of getting Default Band Utilization value for 2.4ghz using webpa  is true "
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	} catch (Exception exception) {
	    errorMessage = "Exception occured during execution !!!!" + exception.getMessage();
	    LOGGER.error(errorMessage);
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, false, errorMessage, false);

	} finally {
	    BroadBandBandSteeringUtils.setBandSteeringPhyThreshold(device, tapEnv, WiFiFrequencyBand.WIFI_BAND_2_GHZ,
		    phyThresholdBeforeSetOperation);
	    LOGGER.info("Parameter reverted to default value ");
	}

	LOGGER.debug("ENDING TESTCASE :testVerifySetAndGetPhysicalRateThresholdFor2GHzRadio() ");

    }
    
    /**
     * Set IdleInactiveTime and OverlaodInactiveTimne to values and verify cfg -s and lbd.conf is
     * updated.
     * 
     * *
     * <ol>
     * <li>STEP 1: Configure band steering per radio using SNMP object rdkbRgDot11BandSteeringBSTable -
     * .1.3.6.1.4.1.17270.50.2.2.8.4.Check if Utilization threshold, RSSI threshold, PHYRate threshold can be
     * individually configured for both radios and both SSIDs.</li>
     * <li>STEP 2:Enable xfinityWifi for both radios using commands</li>
     * <li>STEP 3:Configure the gateway to have same private ssid for both radios</li>
     * <li>STEP 4:Execute a CURL COMMAND to change the
     * Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.1.IdleInactiveTime object to 11</li>
     * <li>STEP 5:Verify lbd.conf is updated with correct values.</li>
     * <li>STEP 6:Verify: cfg -s | grep BS_IS_NORM_INACT_TIMEOUT are updated with correct values.</li>
     * <li>STEP 7:Execute a CURL COMMAND to change the
     * Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.2.IdleInactiveTime object to 13</li>
     * <li>STEP 8:Verify lbd.conf is updated with correct values.</li>
     * <li>STEP 9:cfg -s | grep BS_IS_NORM_INACT_TIMEOUT are updated with correct values.</li>
     * <li>STEP 10:Execute a CURL COMMAND to change the Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.1.
     * OverloadInactiveTime object to 12</li>
     * <li>STEP 11:Verify lbd.conf is updated with correct values.</li>
     * <li>STEP 12:Verify: cfg -s | grep BS_IS_OVERLOAD_INACT_TIMEOUT are updated with correct values. .</li>
     * <li>STEP 13:Execute a CURL COMMAND to change the Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.2.
     * OverloadInactiveTime object to 14</li>
     * <li>STEP 14:Verify lbd.conf is updated with correct values.</li>
     * <li>STEP 15:Verify: cfg -s | grep BS_IS_OVERLOAD_INACT_TIMEOUT are updated with correct values. .</li>
     * 
     * </ol>
     * 
     * @param device
     *            {@link Dut}
     * @author anandam.s
     * @refactor Alan_Bivera
     */
    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
	    BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-WIFI-BAND-STEERING-1011")
    public void testVerifyConfigurationFilesWithSameSSIDForBothRadios(Dut device) {
	// Test case id
	String testId = "TC-RDKB-WIFI-BAND-STEERING-111";
	// Test step number
	String testStepNumber = "s1";
	// result variable
	boolean status = false;
	// String to store the error message
	String errorMessage = null;
	String[] paramList = { BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID,
		BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID };
	Map<String, String> ssidAndPasswordBeforeChange = new HashMap<String, String>();

	BroadBandResultObject result = new BroadBandResultObject();
	String meshInitialStatus = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE);
	boolean isMeshStatusChanged = false;
	try {

	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: " + testId);
	    LOGGER.info(
		    "TEST DESCRIPTION: Set IdleInactiveTime  and OverlaodInactiveTimne to values and  verify cfg -s and lbd.conf is updated");
	    LOGGER.info("*************************************************************************");

	    LOGGER.info("STEP 1: DESCRIPTION : Enable  band steering using SNMP from gateway device  ");
	    LOGGER.info("STEP 2: DESCRIPTION : Enable xfinityWifi for both radios using commands    ");
	    LOGGER.info("STEP 3: DESCRIPTION : Configure the gateway to have same private ssid  for both radios  ");
	    LOGGER.info(
		    "STEP 4: DESCRIPTION : Execute a CURL COMMAND to change the Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.1.IdleInactiveTime object to 11");
	    LOGGER.info("STEP 5: DESCRIPTION : Verify  lbd.conf is updated with correct values. ");
	    LOGGER.info(
		    "STEP 6: DESCRIPTION : Verify: cfg -s | grep BS_IS_NORM_INACT_TIMEOUT are updated with correct values. . ");
	    LOGGER.info(
		    "STEP 7: DESCRIPTION : Execute a CURL COMMAND to change the Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.2.IdleInactiveTime object to 13");
	    LOGGER.info("STEP 8: DESCRIPTION : Verify  lbd.conf is updated with correct values. ");
	    LOGGER.info(
		    "STEP 9: DESCRIPTION : Verify: cfg -s | grep BS_IS_NORM_INACT_TIMEOUT are updated with correct values. . ");
	    LOGGER.info(
		    "STEP 10: DESCRIPTION : Execute a CURL COMMAND to change the Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.1.OverloadInactiveTime object to 12");
	    LOGGER.info("STEP 11: DESCRIPTION : Verify  lbd.conf is updated with correct values. ");
	    LOGGER.info(
		    "STEP 12: DESCRIPTION : Verify: cfg -s | grep BS_IS_OVERLOAD_INACT_TIMEOUT are updated with correct values. . ");
	    LOGGER.info(
		    "STEP 13: DESCRIPTION : Execute a CURL COMMAND to change the Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.2.OverloadInactiveTime object to 14");
	    LOGGER.info("STEP 14: DESCRIPTION : Verify  lbd.conf is updated with correct values. ");
	    LOGGER.info(
		    "STEP 15: DESCRIPTION : Verify: cfg -s | grep BS_IS_OVERLOAD_INACT_TIMEOUT are updated with correct values. . ");
	    if (meshInitialStatus.contains(BroadBandTestConstants.TRUE)) {
		LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
		LOGGER.info("PRE-CONDITION STEPS");
		BroadBandPreConditionUtils.executePreConditionToEnableOrDisable(device, tapEnv, false,
			BroadBandTestConstants.CONSTANT_1);
		isMeshStatusChanged = true;
	    }
	    // get the SSID values before test
	    ssidAndPasswordBeforeChange = tapEnv.executeMultipleWebPaGetCommands(device, paramList);

	    LOGGER.info("******************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Enable  band steering using SNMP from gateway device  ");
	    LOGGER.info(
		    "STEP 1: ACTION      : enabling band steering using SNMP object -rdkbRgDot11BandSteeringEnable [OID- .1.3.6.1.4.1.17270.50.2.2.8.2.0 ] to 1 for true ");
	    LOGGER.info("STEP 1: EXPECTED    : Band steering should  be enabled via SNMP successfully. ");
	    LOGGER.info("******************************************************");
	    errorMessage = "Failed to enable band steering  using snmp from gateway device";
	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_BAND_STEERING.getOid(), SnmpDataType.INTEGER,
		    String.valueOf(BroadBandTestConstants.CONSTANT_1),
		    BroadBandSnmpMib.ECM_BAND_STEERING.getTableIndex());
	    LOGGER.info(
		    "STEP 1 : ACTUAL :" + (status ? "Successfully enabled band steering using snmp " : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    testStepNumber = "s2";
	    status = false;
	    LOGGER.info("******************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Enable xfinityWifi for both radios using commands    ");
	    LOGGER.info("STEP 2: ACTION      : Enable xfinityWifi for both radios using webpa commands ");
	    LOGGER.info("STEP 2: EXPECTED    :Configuration should be successful.");
	    LOGGER.info("******************************************************");
	    errorMessage = "Failed to enable xfinity wifi for both radios";
	    // enable xfinity wifi
	    status = BroadBandWebPaUtils.enablePublicWifiWithSetParameters(device, tapEnv);
	    LOGGER.info("STEP 1 : ACTUAL :"
		    + (status ? "Successfully enabled xfinity wifi for both radios. " : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    errorMessage = "Failed to configure same SSID for both radios";
	    testStepNumber = "s3";
	    status = false;
	    LOGGER.info("******************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Configure the gateway to have same private ssid  for both radios  ");
	    LOGGER.info(
		    "STEP 3: ACTION      : Execute set on webpa Device.WiFi.SSID.10001.SSID  and Device.WiFi.SSID.10101.SSID . Ex: RDKB for 2.4GHz and RDKBfor 5GHz ");
	    LOGGER.info(
		    "STEP 3: EXPECTED    : The CURL COMMAND should get executed successfully and should not show any failure or Fault Code. . ");
	    LOGGER.info("******************************************************");
	    // configure same SSID for both radios
	    Map<String, String> paramMap = new HashMap<String, String>();
	    paramMap.put(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID,
		    BroadBandTestConstants.TEXT_RDKB);
	    paramMap.put(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID,
		    BroadBandTestConstants.TEXT_RDKB);
	    result = BroadBandBandSteeringUtils.configureSSIDForBothRadios(device, tapEnv, paramMap);
	    LOGGER.info("STEP 3 : ACTUAL :"
		    + (result.isStatus() ? "SSID for 2.4Ghz and 5 GHZ set as \"RDKB\" " : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, result.isStatus(), result.getErrorMessage(),
		    true);

	    LOGGER.info("******************************************************");
	    LOGGER.info(
		    "STEP 4: DESCRIPTION : Execute a CURL COMMAND to change the Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.1.IdleInactiveTime object to 11");
	    LOGGER.info(
		    "STEP 4: ACTION      : set webpa Execute a CURL COMMAND to change the Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.1.IdleInactiveTime object to 11 ");
	    LOGGER.info(
		    "STEP 4: EXPECTED    :The CURL COMMAND should get executed successfully and should not show any failure or Fault Code. ");
	    LOGGER.info("******************************************************");
	    testStepNumber = "s4";
	    status = false;
	    errorMessage = "Failed to set webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.1.IdleInactiveTime object to 11";
	    LOGGER.info("waiting for 1 minute for before executing another webpa.");
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_BAND_STEERING_IDLE_INACTIVE_TIME_2_4GHZ,
		    WebPaDataTypes.UNSIGNED_INT.getValue(), BroadBandTestConstants.TEST_VALUE_IDLE_INACTIVE_TIME_2GHZ);
	    LOGGER.info("STEP 4 : ACTUAL :" + (status
		    ? "webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.1.IdleInactiveTime set successfully"
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    LOGGER.info("******************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION : Verify  lbd.conf is updated with correct values. ");
	    LOGGER.info(
		    "STEP 5: ACTION      : Execute :cat /tmp/lbd.conf .Get the values coming under heading [WLANIF2G] ");
	    LOGGER.info("STEP 5: EXPECTED    : lbd.conf should contain InactIdleThreshold=11   ");
	    LOGGER.info("******************************************************");
	    testStepNumber = "s5";
	    result.setStatus(false);
	    result.setErrorMessage("InactIdleThreshold values are not updated properly in lbd.conf file");
	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		result = BroadBandBandSteeringUtils.checkForPatternInLbdConfFile(device, tapEnv,
			WiFiFrequencyBand.WIFI_BAND_2_GHZ, BAND_STEERING_PARAM.BAND_STEERING_PARAM_IDLE_INACTIVE_TIME);
		LOGGER.info("STEP 5 : ACTUAL :"
			+ (result.isStatus() ? "InactIdleThreshold values are updated properly in lbd.conf file."
				: result.getErrorMessage()));
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, result.isStatus(),
			result.getErrorMessage(), false);
	    } else {
		LOGGER.info("STEP 5: ACTUAL: Not applicable for non atom device types");
		tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }

	    LOGGER.info("******************************************************");
	    LOGGER.info(
		    "STEP 6: DESCRIPTION : Verify: cfg -s | grep BS_IS_NORM_INACT_TIMEOUT are updated with correct values. . ");
	    LOGGER.info("STEP 6: ACTION      : Execute : cfg -s | grep BS_IS_NORM_INACT_TIMEOUT");
	    LOGGER.info(
		    "STEP 6: EXPECTED    : Look for BS_IS_NORM_INACT_TIMEOUT:=11  and InactIdleThreshold=11  ( under  2 ghz values ) ");
	    LOGGER.info("******************************************************");
	    testStepNumber = "s6";
	    result.setStatus(false);
	    result.setErrorMessage("InactIdleThreshold values are not updated properly in configuration files");
	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		result = BroadBandBandSteeringUtils.checkForPatternInCfg(device, tapEnv,
			WiFiFrequencyBand.WIFI_BAND_2_GHZ, BAND_STEERING_PARAM.BAND_STEERING_PARAM_IDLE_INACTIVE_TIME);
		LOGGER.info("STEP 6 : ACTUAL :"
			+ (result.isStatus() ? "InactIdleThreshold values are updated properly in configuration files."
				: (result.getErrorMessage())));
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, result.isStatus(),
			result.getErrorMessage(), false);
	    } else {
		LOGGER.info("STEP 6: ACTUAL: Not applicable for non atom device types");
		tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }

	    LOGGER.info("******************************************************");
	    LOGGER.info(
		    "STEP 7: DESCRIPTION : Execute a CURL COMMAND to change the Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.2.IdleInactiveTime object to 13");
	    LOGGER.info(
		    "STEP 7: ACTION      : set webpa Execute a CURL COMMAND to change the Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.2.IdleInactiveTime object to 13 ");
	    LOGGER.info(
		    "STEP 7: EXPECTED    :The CURL COMMAND should get executed successfully and should not show any failure or Fault Code. ");
	    LOGGER.info("******************************************************");
	    testStepNumber = "s7";
	    status = false;
	    errorMessage = "Failed to set webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.2.IdleInactiveTime object to 13";
	    LOGGER.info("waiting for 1 minute for before executing another webpa.");
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_BAND_STEERING_IDLE_INACTIVE_TIME_5GHZ,
		    WebPaDataTypes.UNSIGNED_INT.getValue(), BroadBandTestConstants.TEST_VALUE_IDLE_INACTIVE_TIME_5GHZ);
	    LOGGER.info("STEP 7 : ACTUAL :" + (status
		    ? "webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.2.IdleInactiveTime set successfully"
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    LOGGER.info("******************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION : Verify  lbd.conf is updated with correct values. ");
	    LOGGER.info(
		    "STEP 8: ACTION      : Execute :cat /tmp/lbd.conf Get the values coming under heading [WLANIF5G]  ");
	    LOGGER.info("STEP 8: EXPECTED    : lbd.conf should contain InactIdleThreshold=13 ");
	    LOGGER.info("******************************************************");
	    testStepNumber = "s8";
	    result.setStatus(false);
	    result.setErrorMessage("InactIdleThreshold values are not updated properly in configuration files");
	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		result = BroadBandBandSteeringUtils.checkForPatternInLbdConfFile(device, tapEnv,
			WiFiFrequencyBand.WIFI_BAND_5_GHZ, BAND_STEERING_PARAM.BAND_STEERING_PARAM_IDLE_INACTIVE_TIME);
		LOGGER.info("STEP 8 : ACTUAL :"
			+ (result.isStatus() ? "InactIdleThreshold values are updated properly in lbd.conf files."
				: result.getErrorMessage()));
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, result.isStatus(),
			result.getErrorMessage(), false);
	    } else {
		LOGGER.info("STEP 8: ACTUAL: Not applicable for non atom device types");
		tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }

	    LOGGER.info("******************************************************");
	    LOGGER.info(
		    "STEP 9: DESCRIPTION : Verify: cfg -s | grep BS_IS_NORM_INACT_TIMEOUT are updated with correct values. . ");
	    LOGGER.info("STEP 9: ACTION      : Execute cfg -s | grep BS_IS_NORM_INACT_TIMEOUT  in atom console  ");
	    LOGGER.info("STEP 9: EXPECTED    : Output should contain BS_IS_NORM_INACT_TIMEOUT:=13");
	    LOGGER.info("******************************************************");
	    testStepNumber = "s9";
	    result.setStatus(false);
	    result.setErrorMessage("InactIdleThreshold values are not updated properly in configuration files");
	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		result = BroadBandBandSteeringUtils.checkForPatternInCfg(device, tapEnv,
			WiFiFrequencyBand.WIFI_BAND_5_GHZ, BAND_STEERING_PARAM.BAND_STEERING_PARAM_IDLE_INACTIVE_TIME);
		LOGGER.info("STEP 9 : ACTUAL :"
			+ (result.isStatus() ? "InactIdleThreshold values are updated properly in configuration files."
				: result.getErrorMessage()));
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, result.isStatus(),
			result.getErrorMessage(), false);
	    } else {
		LOGGER.info("STEP 9: ACTUAL: Not applicable for non atom device types");
		tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }

	    LOGGER.info("******************************************************");
	    LOGGER.info(
		    "STEP 10: DESCRIPTION : Execute a CURL COMMAND to change the Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.1.OverloadInactiveTime object to 12");
	    LOGGER.info(
		    "STEP 10: ACTION      : set webpa Execute a CURL COMMAND to change the Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.1.OverloadInactiveTime object to 12 ");
	    LOGGER.info(
		    "STEP 10: EXPECTED    :The CURL COMMAND should get executed successfully and should not show any failure or Fault Code. ");
	    LOGGER.info("******************************************************");
	    testStepNumber = "s10";
	    status = false;
	    errorMessage = "Failed to set webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.1.OverloadInactiveTime object to 12";
	    LOGGER.info("waiting for 1 minute for before executing another webpa.");
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_BAND_STEERING_OVERLOAD_INACTIVE_TIME_2_4GHZ,
		    WebPaDataTypes.UNSIGNED_INT.getValue(),
		    BroadBandTestConstants.TEST_VALUE_OVERLOAD_INACTIVE_TIME_2GHZ);
	    LOGGER.info("STEP 10 : ACTUAL :" + (status
		    ? "webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.1.OverloadInactiveTime set successfully"
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    LOGGER.info("******************************************************");
	    LOGGER.info("STEP 11: DESCRIPTION : Verify  lbd.conf is updated with correct values. ");
	    LOGGER.info(
		    "STEP 11: ACTION      : Execute :cat /tmp/lbd.conf .Get the values coming under heading [WLANIF2G] ");
	    LOGGER.info("STEP 11: EXPECTED    : lbd.conf should contain InactOverloadThreshold=12 ");
	    LOGGER.info("******************************************************");
	    testStepNumber = "s11";
	    result.setStatus(false);
	    result.setErrorMessage("OverloadIdleThreshold values are not updated properly in configuration files");
	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		result = BroadBandBandSteeringUtils.checkForPatternInLbdConfFile(device, tapEnv,
			WiFiFrequencyBand.WIFI_BAND_2_GHZ,
			BAND_STEERING_PARAM.BAND_STEERING_PARAM_OVERLOAD_INACTIVE_TIME);
		LOGGER.info("STEP 11 : ACTUAL :" + (result.isStatus()
			? "OverloadIdleThreshold values are updated properly in configuration files."
			: result.getErrorMessage()));
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, result.isStatus(),
			result.getErrorMessage(), false);
	    } else {
		LOGGER.info("STEP 11: ACTUAL: Not applicable for non atom device types");
		tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }

	    LOGGER.info("******************************************************");
	    LOGGER.info(
		    "STEP 12: DESCRIPTION : Verify: cfg -s | grep BS_IS_OVERLOAD_INACT_TIMEOUT are updated with correct values. . ");
	    LOGGER.info("STEP 12: ACTION      : Execute cfg -s | grep BS_IS_OVERLOAD_INACT_TIMEOUT  ");
	    LOGGER.info("STEP 12: EXPECTED    : Output should contain BS_IS_OVERLOAD_INACT_TIMEOUT:=12 ");
	    LOGGER.info("******************************************************");
	    testStepNumber = "s12";
	    result.setStatus(false);
	    result.setErrorMessage("OverloadIdleThreshold values are not updated properly in configuration files");
	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		result = BroadBandBandSteeringUtils.checkForPatternInCfg(device, tapEnv,
			WiFiFrequencyBand.WIFI_BAND_2_GHZ,
			BAND_STEERING_PARAM.BAND_STEERING_PARAM_OVERLOAD_INACTIVE_TIME);
		LOGGER.info("STEP 12 : ACTUAL :" + (result.isStatus()
			? "OverloadIdleThreshold values are updated properly in configuration files."
			: result.getErrorMessage()));
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, result.isStatus(),
			result.getErrorMessage(), false);
	    } else {
		LOGGER.info("STEP 12: ACTUAL: Not applicable for non atom device types");
		tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }

	    LOGGER.info("******************************************************");
	    LOGGER.info(
		    "STEP 13: DESCRIPTION : Execute a CURL COMMAND to change the Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.2.OverloadInactiveTime object to 14");
	    LOGGER.info(
		    "STEP 13: ACTION      : set webpa Execute a CURL COMMAND to change the Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.2.OverloadInactiveTime object to 14");
	    LOGGER.info(
		    "STEP 13: EXPECTED    :The CURL COMMAND should get executed successfully and should not show any failure or Fault Code. ");
	    LOGGER.info("******************************************************");
	    testStepNumber = "s13";
	    status = false;
	    errorMessage = "Failed to set webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.2.OverloadInactiveTime object to 14";
	    LOGGER.info("waiting for 1 minute for before executing another webpa.");
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_BAND_STEERING_OVERLOAD_INACTIVE_TIME_5GHZ,
		    WebPaDataTypes.UNSIGNED_INT.getValue(),
		    BroadBandTestConstants.TEST_VALUE_OVERLOAD_INACTIVE_TIME_5GHZ);
	    LOGGER.info("STEP 13 : ACTUAL :" + (status
		    ? "webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.2.OverloadInactiveTime set successfully"
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    LOGGER.info("******************************************************");
	    LOGGER.info("STEP 14: DESCRIPTION : Verify  lbd.conf is updated with correct values. ");
	    LOGGER.info(
		    "STEP 14: ACTION      : Execute :cat /tmp/lbd.conf in atom console .Get the values coming under heading [WLANIF5G]  ");
	    LOGGER.info("STEP 14: EXPECTED    : lbd.conf should contain InactOverloadThreshold=14  ");
	    LOGGER.info("******************************************************");
	    testStepNumber = "s14";
	    result.setStatus(false);
	    result.setErrorMessage("OverloadIdleThreshold values are not updated properly in configuration files");
	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		result = BroadBandBandSteeringUtils.checkForPatternInLbdConfFile(device, tapEnv,
			WiFiFrequencyBand.WIFI_BAND_5_GHZ,
			BAND_STEERING_PARAM.BAND_STEERING_PARAM_OVERLOAD_INACTIVE_TIME);
		LOGGER.info("STEP 14 : ACTUAL :" + (result.isStatus()
			? "OverloadIdleThreshold values are updated properly in configuration files."
			: result.getErrorMessage()));
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, result.isStatus(),
			result.getErrorMessage(), false);
	    } else {
		LOGGER.info("STEP 14: ACTUAL: Not applicable for non atom device types");
		tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }

	    LOGGER.info("******************************************************");
	    LOGGER.info(
		    "STEP 15: DESCRIPTION : Verify: cfg -s | grep BS_IS_OVERLOAD_INACT_TIMEOUT are updated with correct values. . ");
	    LOGGER.info("STEP 15: ACTION      : Execute cfg -s | grep BS_IS_OVERLOAD_INACT_TIMEOUT in atom console  ");
	    LOGGER.info("STEP 15: EXPECTED    : Output should contain BS_IS_OVERLOAD_INACT_TIMEOUT:=14 ");
	    LOGGER.info("******************************************************");
	    testStepNumber = "s15";
	    result.setStatus(false);
	    result.setErrorMessage("OverloadIdleThreshold values are not updated properly in configuration files");
	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		result = BroadBandBandSteeringUtils.checkForPatternInCfg(device, tapEnv,
			WiFiFrequencyBand.WIFI_BAND_5_GHZ,
			BAND_STEERING_PARAM.BAND_STEERING_PARAM_OVERLOAD_INACTIVE_TIME);
		LOGGER.info("STEP 15 : ACTUAL :" + (result.isStatus()
			? "OverloadIdleThreshold values are updated properly in configuration files."
			: result.getErrorMessage()));
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, result.isStatus(),
			result.getErrorMessage(), false);
	    } else {
		LOGGER.info("STEP 15: ACTUAL: Not applicable for non atom device types");
		tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }

	} catch (Exception exception) {
	    errorMessage = "Exception occured during execution !!!!" + exception.getMessage();
	    LOGGER.error(errorMessage);
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, false, errorMessage, false);

	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("POST-CONDITION 1: DESCRIPTION : REVERT BACK DEFAULT VALUES ");
	    LOGGER.info("POST-CONDITION  1: ACTION :EXECUTE WEBPA PARAM TO REVERT SSID AND PASSWORD ");
	    LOGGER.info("POST-CONDITION 1: EXPTECTED : VALUES REVERTED TO DEFAULT VALUES");
	    LOGGER.info("#######################################################################################");
	    // set old values
	    List<WebPaParameter> webPaParameters = new ArrayList<WebPaParameter>();
	    for (String param : paramList) {
		WebPaParameter webPaParameter = BroadBandWebPaUtils.generateWebpaParameterWithValueAndType(param,
			ssidAndPasswordBeforeChange.get(param), WebPaDataTypes.STRING.getValue());
		webPaParameters.add(webPaParameter);

	    }
	    tapEnv.executeMultipleWebPaSetCommands(device, webPaParameters);
	    LOGGER.info("Waiting for 90 seconds to disable xfinity wifi");
	    tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);
	    // disabling the xfinity wifi
	    boolean isPublicWifiDisabled = BroadBandWiFiUtils.setWebPaParams(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_ENABLING_PUBLIC_WIFI, BroadBandTestConstants.FALSE,
		    BroadBandTestConstants.CONSTANT_3);
	    LOGGER.info("DISABLING THE PUBLIC WIFI ON THIS DEVICE-" + isPublicWifiDisabled);
	    if (!isPublicWifiDisabled) {
		LOGGER.error("DISABLING PUBLIC WIFI FAILED.");
	    }
	    if (isMeshStatusChanged) {
		LOGGER.info("#######################################################################################");
		LOGGER.info("POST-CONDITION 2: DESCRIPTION : REVERT BACK MESH VALUE ");
		LOGGER.info("POST-CONDITION 2 : ACTION :EXECUTE XPC TO REVERT BACK MESH VALUES ");
		LOGGER.info("POST-CONDITION 2 : EXPTECTED : MESH REVERTED TO ORIGINAL VALUE SUCCESSFULLY");
		LOGGER.info("#######################################################################################");
		status = BroadBandMeshUtils.enableOrDisableMesh(device, tapEnv, false);
		if (status) {
		    LOGGER.info("POST-CONDITION : ACTUAL :Mesh status reverted back sucessfully");
		} else {
		    LOGGER.error("POST-CONDITION : ACTUAL :Failed to revert back mesh status");
		}

	    }
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}

	LOGGER.debug("ENDING TESTCASE :testVerifyConfigurationFilesWithSameSSIDForBothRadios() ");

    }
    
}
