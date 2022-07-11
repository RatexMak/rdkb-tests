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

import org.codehaus.jettison.json.JSONObject;
import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.constants.LinuxCommandConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.TestCategory;
import com.automatics.rdkb.constants.BroadBandCdlConstants;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandConnectedClientTestConstants;
import com.automatics.rdkb.constants.BroadBandPropertyKeyConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants.BAND_STEERING_PARAM;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants.RdkBBandSteeringParameters;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;
import com.automatics.rdkb.constants.WebPaParamConstants;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.enums.BroadBandMFPConfigEnum;
import com.automatics.rdkb.enums.BroadBandManagementPowerControlEnum;
import com.automatics.rdkb.tests.snmp.BroadBandSnmpTest;
import com.automatics.rdkb.tests.wifi.BroadbandRadioStatusWifiTest;
import com.automatics.rdkb.utils.BroadBandBandSteeringUtils;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandMeshUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.BroadBandSystemUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.cdl.BroadBandCodeDownloadUtils;
import com.automatics.rdkb.utils.cdl.FirmwareDownloadUtils;
import com.automatics.rdkb.utils.dmcli.DmcliUtils;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpMib;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandMgmtFramePwrControlUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.rdkb.webui.BroadBandWebUiBaseTest;
import com.automatics.rdkb.webui.page.BroadBandWifiPage.SSIDFrequency;
import com.automatics.snmp.SnmpDataType;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.utils.CommonMethods;
import com.automatics.webpa.WebPaParameter;
import com.automatics.webpa.WebPaServerResponse;
import com.automatics.utils.AutomaticsPropertyUtility;

/**
 * Test class with test case for validating web ui based tests
 * 
 * @Refactor Alan_Bivera
 */
public class BroadBandMsoWebGuiTest extends BroadBandWebUiBaseTest {
	
	/** Constant holds the test step number with S **/
    private static String stepNum = null;
    
    private String ERR_MSG_WITH_VALUE = " with value ";
    
    /** Constant holds the test status **/
    private static boolean status = false;
    
    /** Constant holds the errormessage **/
    private static String errorMessage = null;



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
		LOGGER.info(
			"STEP 8: ACTUAL : Log message is present with reason when beacon rate is changed to 12Mbps");
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
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, resultObject.isStatus(), errorMessage, false);

	    if (DeviceModeHandler.isDSLDevice(device) || DeviceModeHandler.isFibreDevice(device)
		    || CommonMethods.isAtomSyncAvailable(device, tapEnv)) {

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
			RdkBBandSteeringParameters.DEFAULT_STEERING_IDLE_INACTIVE_TIME_2GHZ.getDefaultValue(), true,
			bandSteeringWebpaResponse);
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
			RdkBBandSteeringParameters.DEFAULT_STEERING_IDLE_INACTIVE_TIME_5GHZ.getDefaultValue(), true,
			bandSteeringWebpaResponse);
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
			RdkBBandSteeringParameters.DEFAULT_STEERING_OVERLOAD_INACTIVE_TIME_2GHZ.getDefaultValue(), true,
			bandSteeringWebpaResponse);
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
			RdkBBandSteeringParameters.DEFAULT_STEERING_OVERLOAD_INACTIVE_TIME_5GHZ.getDefaultValue(), true,
			bandSteeringWebpaResponse);
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

	} finally {
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
     * Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.2.PhyRateThreshold object to desired threshold to be
     * set.</li>
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
     * Device.WiFi.X_RDKCENTRAL-COM_BandSteering.BandSetting.1.PhyRateThreshold object to desired threshold to be
     * set.</li>
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
     * Set IdleInactiveTime and OverlaodInactiveTimne to values and verify cfg -s and lbd.conf is updated.
     * 
     * *
     * <ol>
     * <li>STEP 1: Configure band steering per radio using SNMP object rdkbRgDot11BandSteeringBSTable -
     * .1.3.6.1.4.1.17270.50.2.2.8.4.Check if Utilization threshold, RSSI threshold, PHYRate threshold can be
     * individually configured for both radios and both SSIDs.</li>
     * <li>STEP 2:Enable publicWifi for both radios using commands</li>
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
	    LOGGER.info("STEP 2: DESCRIPTION : Enable public Wifi for both radios using commands    ");
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
	    LOGGER.info("STEP 2: DESCRIPTION : Enable public Wifi for both radios using commands    ");
	    LOGGER.info("STEP 2: ACTION      : Enable public Wifi for both radios using webpa commands ");
	    LOGGER.info("STEP 2: EXPECTED    :Configuration should be successful.");
	    LOGGER.info("******************************************************");
	    errorMessage = "Failed to enable public wifi for both radios";
	    // enable public wifi
	    status = BroadBandWebPaUtils.enablePublicWifiWithSetParameters(device, tapEnv);
	    LOGGER.info("STEP 1 : ACTUAL :"
		    + (status ? "Successfully enabled public wifi for both radios. " : errorMessage));
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
	    LOGGER.info("Waiting for 90 seconds to disable public wifi");
	    tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);
	    // disabling the public wifi
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

    /**
     * Test case is created as part of COVERAGE AUTOMATION based on the Management Frame Power control for 2.4 and 5 GHz
     * (Per SSID).
     *
     * Test Case # 1: Verify Management Frame Power Controls for 2.4 GHz & 5 GHz Radios using WebPA.
     *
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>S1) Verify retrieving the management frame power level for 2.4 GHz Private WiFi Access Point (1).</li>
     * <li>S2) Verify setting the management frame power level for 2.4 GHz Private WiFi Access Point (1) with valid
     * value.</li>
     * <li>S3) Verify the logging for setting the value for the 2.4 GHz Private WiFi Access Point's Management Frame
     * (1)</li>
     * <li>S4) Verify setting the management frame power level for 2.4 GHz Private WiFi Access Point (1) with invalid
     * value 5.</li>
     * <li>S5) Verify retrieving the management frame power level for 2.4 GHz Private WiFi Access Point (1) returns
     * 0</li>
     * <li>S6) Verify setting the management frame power level for 2.4 GHz Private WiFi Access Point (1) with invalid
     * value -25</li>
     * <li>S7) Verify retrieving the management frame power control for 2.4 GHz Private WiFi Access Point (1) returns
     * -20</li>
     * <li>S8) Verify retrieving the management frame power level for 5 GHz Private WiFi Access Point (2).</li>
     * <li>S9) Verify setting the management frame power control for 5 GHz Private WiFi Access Point (2) with valid
     * value.</li>
     * <li>S10) Verify the logging for setting the value for the 5 GHz Private WiFi Access Point's management frame
     * (2)</li>
     * <li>S11) Verify setting the management frame power level for 5 GHz Private WiFi Access Point (2) with invalid
     * value 5.</li>
     * <li>S12) Verify retrieving the management frame power level for 5 GHz Private WiFi Access Point (2) returns
     * 0</li>
     * <li>S13) Verify setting the management frame power level for 5 GHz Private WiFi Access Point (1) with invalid
     * value -25</li>
     * <li>S14) Verify retrieving the management frame power level for 5 GHz Private WiFi Access Point (1) returns
     * -20</li>
     * <li>S15) Verify setting the power values for all the 2.4 GHz WiFi Access Point's Management Frames</li>
     * <li>S16) Verify retrieving the power values for all the 2.4 GHz WiFi Access Point's Management Frames</li>
     * <li>S17) Verify setting the power values for all the 5 GHz WiFi Access Point's Management Frames</li>
     * <li>S18) Verify retrieving the power values for all the 5 GHz WiFi Access Point's Management Frames</li>
     * <li>S19) Verify the 2.4 GHz WiFi Access Point's Management Frames power values are persisted after reboot.</li>
     * <li>S20) Verify the 5 GHz WiFi Access Point's Management Frames power values are persisted after reboot.</li>
     * <li>S21) Verify the 2.4 GHz WiFi Access Point's Management Frames power values are persisted after image
     * upgrade.</li>
     * <li>S22) Verify the 5 GHz WiFi Access Point's Management Frames power values are persisted after image
     * upgrade.</li>
     * <li>S23) Verify the 2.4 GHz WiFi Access Point's Management Frames power values are set to 0 on performing Factory
     * Reset.</li>
     * <li>S24) Verify the 5 GHz WiFi Access Point's Management Frames power values are set to 0 on performing Factory
     * Reset.</li>
     * </ol>
     *
     * @author BALAJI V
     * @refactor Alan_Bivera
     * 
     * @param device
     *            {@link Dut}
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
	    BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-WIFI-5051")
    public void testMgmtFramePwrControlWebPa(Dut device) {
	String testCaseId = "TC-RDKB-WIFI-551";
	boolean result = false;
	boolean hasBuildChanged = false;
	boolean hasFactoryReset = false;
	String step = null;
	String errorMessage = null;
	String currentFirmwareVersion = null;
	try {
	    int stepNumber = 1;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-5051");
	    LOGGER.info(
		    "TEST DESCRIPTION: Verify Management Frame Power Controls for 2.4 GHz & 5 GHz Radios using WebPA.");

	    /**
	     * S1) Verify retrieving the management frame power level for 2.4 GHz Private WiFi Access Point (1).
	     */
	    step = "S" + stepNumber;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": VERIFY THE MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz PRIVATE WIFI ACCESS POINT.");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : VALUE MUST BE IN RANGE -20 - 0.");
	    String parameterName = BroadBandManagementPowerControlEnum.MGMT_PWR_CTRL_WIFI_AP_1.getWebPaParamMgmtPower();
	    String response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv, parameterName);
	    errorMessage = "UNABLE TO VERIFY THE MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz PRIVATE WIFI ACCESS POINT: ACTUAL VALUE = "
		    + response;
	    result = CommonMethods.isNotNull(response)
		    && BroadBandMgmtFramePwrControlUtils.verifyPowerLevelIsWithinRange(response);
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
		    + (result ? "VERIFIED THE MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz PRIVATE ACCESS POINT: " + response
			    : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * S2) Verify setting the management frame power level for 2.4 GHz Private WiFi Access Point (1) with valid
	     * value.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    String expectedValue = "-10";
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": VERIFY SETTING THE MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz PRIVATE WIFI ACCESS POINT (VALID VALUE).");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : VALUE MUST BE SET SUCCESSFULLY.");
	    parameterName = BroadBandManagementPowerControlEnum.MGMT_PWR_CTRL_WIFI_AP_1.getWebPaParamMgmtPower();
	    errorMessage = "UNABLE TO SET THE MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz PRIVATE WIFI ACCESS POINT TO: "
		    + expectedValue;
	    result = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv, parameterName,
		    BroadBandTestConstants.CONSTANT_1, expectedValue);
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
		    + (result ? "VERIFIED SETTING THE MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz PRIVATE ACCESS POINT."
			    : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * S3) Verify the logging for setting the value for the 2.4 GHz Private WiFi Access Point's Management Frame
	     * (1)
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": VERIFY LOGGING FOR SETTING MANAGEMENT FRAME POWER CONTROL - 2.4GHz WIFI ACCESS POINT.");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : LOG MESSAGE MUST BE PRESENT WITH THE VALUE SET.");
	    parameterName = BroadBandManagementPowerControlEnum.MGMT_PWR_CTRL_WIFI_AP_1.getWebPaParamMgmtPower();
	    errorMessage = "UNABLE TO VERIFY THE LOG MESSAGE FOR SETTING THE MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz PRIVATE WIFI ACCESS POINT TO: "
		    + expectedValue;
	    response = BroadBandCommonUtils.searchArmOrAtomLogFile(tapEnv, device,
		    BroadBandCommonUtils.concatStringUsingStringBuffer(
			    BroadBandTraceConstants.LOG_MESSAGE_MGMT_FRAME_POWER_CONTROL, expectedValue),
		    BroadBandTestConstants.LOCATION_WIFI_LOG);
	    result = CommonMethods.isNotNull(response);
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : " + (result
		    ? "VERIFIED LOGGING OF SETTING THE MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz PRIVATE ACCESS POINT - "
			    + response
		    : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * S4) Verify setting the management frame power level for 2.4 GHz Private WiFi Access Point (1) with
	     * invalid value 5.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    expectedValue = "5";
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": VERIFY SETTING THE MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz PRIVATE WIFI ACCESS POINT (INVALID VALUE 5).");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : VALUE MUST BE SET SUCCESSFULLY.");
	    parameterName = BroadBandManagementPowerControlEnum.MGMT_PWR_CTRL_WIFI_AP_1.getWebPaParamMgmtPower();
	    errorMessage = "UNABLE TO SET THE MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz PRIVATE WIFI ACCESS POINT TO: "
		    + expectedValue;
	    result = BroadBandWiFiUtils.setWebPaParams(device, parameterName, expectedValue,
		    BroadBandTestConstants.CONSTANT_1);
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
		    + (result ? "VERIFIED SETTING THE MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz PRIVATE ACCESS POINT."
			    : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * S5) Verify retrieving the management frame power level for 2.4 GHz Private WiFi Access Point (1) returns
	     * 0
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    expectedValue = "0";
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": VERIFY THE MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz PRIVATE WIFI ACCESS POINT SET TO 0.");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : VALUE MUST BE SET TO 0.");
	    parameterName = BroadBandManagementPowerControlEnum.MGMT_PWR_CTRL_WIFI_AP_1.getWebPaParamMgmtPower();
	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv, parameterName);
	    errorMessage = "UNABLE TO VERIFY THE MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz PRIVATE WIFI ACCESS POINT: EXPECTED VALUE = "
		    + expectedValue + ", ACTUAL VALUE = " + response;
	    result = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(expectedValue);
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
		    + (result ? "VERIFIED THE MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz PRIVATE ACCESS POINT SET TO 0."
			    : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * S6) Verify setting the management frame power level for 2.4 GHz Private WiFi Access Point (1) with
	     * invalid value -25.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    expectedValue = "-25";
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": VERIFY SETTING THE MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz PRIVATE WIFI ACCESS POINT (INVALID VALUE -25).");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : VALUE MUST BE SET SUCCESSFULLY.");
	    parameterName = BroadBandManagementPowerControlEnum.MGMT_PWR_CTRL_WIFI_AP_1.getWebPaParamMgmtPower();
	    errorMessage = "UNABLE TO SET THE MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz PRIVATE WIFI ACCESS POINT TO: "
		    + expectedValue;
	    result = BroadBandWiFiUtils.setWebPaParams(device, parameterName, expectedValue,
		    BroadBandTestConstants.CONSTANT_1);
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
		    + (result ? "VERIFIED SETTING THE MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz PRIVATE ACCESS POINT."
			    : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * S7) Verify retrieving the management frame power level for 2.4 GHz Private WiFi Access Point (1) returns
	     * 0
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    expectedValue = "-20";
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": VERIFY THE MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz PRIVATE WIFI ACCESS POINT SET TO -20.");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : VALUE MUST BE SET TO -20.");
	    parameterName = BroadBandManagementPowerControlEnum.MGMT_PWR_CTRL_WIFI_AP_1.getWebPaParamMgmtPower();
	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv, parameterName);
	    errorMessage = "UNABLE TO VERIFY THE MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz PRIVATE WIFI ACCESS POINT: EXPECTED VALUE = "
		    + expectedValue + ", ACTUAL VALUE = " + response;
	    result = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(expectedValue);
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
		    + (result ? "VERIFIED THE MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz PRIVATE ACCESS POINT SET TO -20."
			    : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * S8) Verify retrieving the management frame power level for 5GHz Private WiFi Access Point (2).
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": VERIFY THE MANAGEMENT FRAME POWER LEVEL FOR 5GHz PRIVATE WIFI ACCESS POINT.");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : VALUE MUST BE IN RANGE 0 - -20.");
	    parameterName = BroadBandManagementPowerControlEnum.MGMT_PWR_CTRL_WIFI_AP_9.getWebPaParamMgmtPower();
	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv, parameterName);
	    errorMessage = "UNABLE TO VERIFY THE MANAGEMENT FRAME POWER LEVEL FOR 5GHz PRIVATE WIFI ACCESS POINT: ACTUAL VALUE = "
		    + response;
	    result = CommonMethods.isNotNull(response)
		    && BroadBandMgmtFramePwrControlUtils.verifyPowerLevelIsWithinRange(response);
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
		    + (result ? "VERIFIED THE MANAGEMENT FRAME POWER LEVEL FOR 5GHz PRIVATE ACCESS POINT: " + response
			    : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * S9) Verify setting the management frame power level for 5 GHz Private WiFi Access Point (2) with valid
	     * value.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    expectedValue = "-15";
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": VERIFY SETTING THE MANAGEMENT FRAME POWER LEVEL FOR 5GHz PRIVATE WIFI ACCESS POINT (VALID VALUE).");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : VALUE MUST BE SET SUCCESSFULLY.");
	    parameterName = BroadBandManagementPowerControlEnum.MGMT_PWR_CTRL_WIFI_AP_9.getWebPaParamMgmtPower();
	    errorMessage = "UNABLE TO SET THE MANAGEMENT FRAME POWER LEVEL FOR 5GHz PRIVATE WIFI ACCESS POINT TO: "
		    + expectedValue;
	    result = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv, parameterName,
		    BroadBandTestConstants.CONSTANT_1, expectedValue);
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
		    + (result ? "VERIFIED SETTING THE MANAGEMENT FRAME POWER LEVEL FOR 5GHz PRIVATE ACCESS POINT."
			    : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * S10) Verify the logging for setting the value for the 5 GHz Private WiFi Access Point's Management Frame
	     * (2)
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": VERIFY LOGGING FOR SETTING MANAGEMENT FRAME POWER CONTROL - 5GHz WIFI ACCESS POINT.");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : LOG MESSAGE MUST BE PRESENT WITH THE VALUE SET.");
	    parameterName = BroadBandManagementPowerControlEnum.MGMT_PWR_CTRL_WIFI_AP_9.getWebPaParamMgmtPower();
	    errorMessage = "UNABLE TO VERIFY THE LOG MESSAGE FOR SETTING THE MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz PRIVATE WIFI ACCESS POINT TO: "
		    + expectedValue;
	    response = BroadBandCommonUtils.searchArmOrAtomLogFile(tapEnv, device,
		    BroadBandCommonUtils.concatStringUsingStringBuffer(
			    BroadBandTraceConstants.LOG_MESSAGE_MGMT_FRAME_POWER_CONTROL, expectedValue),
		    BroadBandTestConstants.LOCATION_WIFI_LOG);
	    result = CommonMethods.isNotNull(response);
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : " + (result
		    ? "VERIFIED LOGGING OF SETTING THE MANAGEMENT FRAME POWER LEVEL FOR 5GHz PRIVATE ACCESS POINT - "
			    + response
		    : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * S11) Verify setting the management frame power level for 5 GHz Private WiFi Access Point (2) with invalid
	     * value 5.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    expectedValue = "5";
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": VERIFY SETTING THE MANAGEMENT FRAME POWER LEVEL FOR 5GHz PRIVATE WIFI ACCESS POINT (INVALID VALUE 5).");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : VALUE MUST BE SET SUCCESSFULLY.");
	    parameterName = BroadBandManagementPowerControlEnum.MGMT_PWR_CTRL_WIFI_AP_9.getWebPaParamMgmtPower();
	    errorMessage = "UNABLE TO SET THE MANAGEMENT FRAME POWER LEVEL FOR 5GHz PRIVATE WIFI ACCESS POINT TO: "
		    + expectedValue;
	    result = BroadBandWiFiUtils.setWebPaParams(device, parameterName, expectedValue,
		    BroadBandTestConstants.CONSTANT_1);
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
		    + (result ? "VERIFIED SETTING THE MANAGEMENT FRAME POWER LEVEL FOR 5GHz PRIVATE ACCESS POINT."
			    : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * S12) Verify retrieving the management frame power level for 5 GHz Private WiFi Access Point (2) returns 0
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    expectedValue = "0";
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": VERIFY THE MANAGEMENT FRAME POWER LEVEL FOR 5GHz PRIVATE WIFI ACCESS POINT SET TO 0.");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : VALUE MUST BE SET TO 0.");
	    parameterName = BroadBandManagementPowerControlEnum.MGMT_PWR_CTRL_WIFI_AP_9.getWebPaParamMgmtPower();
	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv, parameterName);
	    errorMessage = "UNABLE TO VERIFY THE MANAGEMENT FRAME POWER LEVEL FOR 5GHz PRIVATE WIFI ACCESS POINT: EXPECTED VALUE = "
		    + expectedValue + ", ACTUAL VALUE = " + response;
	    result = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(expectedValue);
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
		    + (result ? "VERIFIED THE MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz PRIVATE ACCESS POINT SET TO 0"
			    : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * S13) Verify setting the management frame power level for 5 GHz Private WiFi Access Point (2) with invalid
	     * value -25.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    expectedValue = "-25";
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": VERIFY SETTING THE MANAGEMENT FRAME POWER LEVEL FOR 5GHz PRIVATE WIFI ACCESS POINT (INVALID VALUE -25).");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : VALUE MUST BE SET SUCCESSFULLY.");
	    parameterName = BroadBandManagementPowerControlEnum.MGMT_PWR_CTRL_WIFI_AP_9.getWebPaParamMgmtPower();
	    errorMessage = "UNABLE TO SET THE MANAGEMENT FRAME POWER LEVEL FOR 5GHz PRIVATE WIFI ACCESS POINT TO: "
		    + expectedValue;
	    result = BroadBandWiFiUtils.setWebPaParams(device, parameterName, expectedValue,
		    BroadBandTestConstants.CONSTANT_1);
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
		    + (result ? "VERIFIED SETTING THE MANAGEMENT FRAME POWER LEVEL FOR 5GHz PRIVATE ACCESS POINT."
			    : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * S14) Verify retrieving the management frame power level for 5 GHz Private WiFi Access Point (2) returns 0
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    expectedValue = "-20";
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": VERIFY THE MANAGEMENT FRAME POWER LEVEL FOR 5GHz PRIVATE WIFI ACCESS POINT SET TO -20.");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : VALUE MUST BE SET TO -20.");
	    parameterName = BroadBandManagementPowerControlEnum.MGMT_PWR_CTRL_WIFI_AP_9.getWebPaParamMgmtPower();
	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv, parameterName);
	    errorMessage = "UNABLE TO VERIFY THE MANAGEMENT FRAME POWER LEVEL FOR 5GHz PRIVATE WIFI ACCESS POINT: EXPECTED VALUE = "
		    + expectedValue + ", ACTUAL VALUE = " + response;
	    result = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(expectedValue);
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
		    + (result ? "VERIFIED THE MANAGEMENT FRAME POWER LEVEL FOR 5GHz PRIVATE ACCESS POINT SET TO -20."
			    : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * S15) Verify setting the power values for all the 2.4 GHz WiFi Access Point's Management Frames
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": VERIFY SETTING THE POWER LEVELS FOR 2.4GHz WIFI ACCESS POINT MANAGEMENT FRAMES.");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : VALUES MUST BE SET SUCCESSFULLY.");
	    List<BroadBandManagementPowerControlEnum> wifiAccessPoints = BroadBandMgmtFramePwrControlUtils
		    .getWifiAccessPointsBasedOnRadio(false);
	    BroadBandResultObject resultObj = BroadBandMgmtFramePwrControlUtils.setMgmtFramePowerLevelsWebPa(tapEnv,
		    device, wifiAccessPoints, false);
	    result = resultObj.isStatus();
	    errorMessage = resultObj.getErrorMessage();
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
		    + (result ? "SET THE MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz WIFI ACCESS POINTS." : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * S16) Verify retrieving the power values for all the 2.4 GHz WiFi Access Point's Management Frames
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": VERIFY RETRIEVING THE POWER LEVELS FOR 2.4GHz WIFI ACCESS POINT MANAGEMENT FRAMES.");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : VALUES MUST BE SAME AS THE VALUES SET IN PREVIOUS STEP.");
	    resultObj = BroadBandMgmtFramePwrControlUtils.verifyMgmtPowerLevels(tapEnv, device, wifiAccessPoints,
		    false);
	    result = resultObj.isStatus();
	    errorMessage = resultObj.getErrorMessage();
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
		    + (result ? "VERIFIED THE VALUES OF MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz WIFI ACCESS POINTS."
			    : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * S17) Verify setting the power values for all the 5 GHz WiFi Access Point's Management Frames
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": VERIFY SETTING THE POWER LEVELS FOR 5GHz WIFI ACCESS POINT MANAGEMENT FRAMES.");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : VALUES MUST BE SET SUCCESSFULLY.");
	    wifiAccessPoints = BroadBandMgmtFramePwrControlUtils.getWifiAccessPointsBasedOnRadio(true);
	    resultObj = BroadBandMgmtFramePwrControlUtils.setMgmtFramePowerLevelsWebPa(tapEnv, device, wifiAccessPoints,
		    false);
	    result = resultObj.isStatus();
	    errorMessage = resultObj.getErrorMessage();
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
		    + (result ? "SET THE MANAGEMENT FRAME POWER LEVEL FOR 5GHz WIFI ACCESS POINTS." : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * S18) Verify retrieving the power values for all the 5 GHz WiFi Access Point's Management Frames
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": VERIFY RETRIEVING THE POWER LEVELS FOR 5GHz WIFI ACCESS POINT MANAGEMENT FRAMES.");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : VALUES MUST BE SAME AS THE VALUES SET IN PREVIOUS STEP.");
	    resultObj = BroadBandMgmtFramePwrControlUtils.verifyMgmtPowerLevels(tapEnv, device, wifiAccessPoints,
		    false);
	    result = resultObj.isStatus();
	    errorMessage = resultObj.getErrorMessage();
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
		    + (result ? "VERIFIED THE VALUES OF MANAGEMENT FRAME POWER LEVEL FOR 5GHz WIFI ACCESS POINTS."
			    : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * S19) Verify the 2.4 GHz WiFi Access Point's Management Frames power values are persisted after reboot.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": VERIFY 2.4GHz WIFI ACCESS POINT MANAGEMENT FRAMES POWER LEVELS PERSISTED AFTER REBOOT.");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : VALUES MUST BE PERSISTED AFTER REBOOT.");
	    boolean isRebooted = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
	    errorMessage = "UNABLE TO REBOOT THE DEVICE.";
	    if (isRebooted) {
		isRebooted = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
		errorMessage = "UNABLE TO VERIFY WEBPA PROCESS IS UP & RUNNING.";
	    }
	    if (isRebooted) {
		wifiAccessPoints = BroadBandMgmtFramePwrControlUtils.getWifiAccessPointsBasedOnRadio(false);
		resultObj = BroadBandMgmtFramePwrControlUtils.verifyMgmtPowerLevels(tapEnv, device, wifiAccessPoints,
			false);
		result = resultObj.isStatus();
		errorMessage = "UNABLE TO VERIFY THE 2.4GHz WIFI ACCESS POINT MANAGEMENT FRAMES POWER LEVELS PERSIST ON REBOOT: "
			+ resultObj.getErrorMessage();
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	    } else {
		tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_TESTED, errorMessage,
			false);
	    }
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : " + (result
		    ? "VERIFIED THE VALUES OF MANAGEMENT FRAME POWER LEVEL FOR 2.4GGHz WIFI ACCESS POINTS PERSIST ON REBOOT."
		    : errorMessage));
	    LOGGER.info("#######################################################################################");

	    /**
	     * S20) Verify the 5 GHz WiFi Access Point's Management Frames power values are persisted after reboot.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": VERIFY 5GHz WIFI ACCESS POINT MANAGEMENT FRAMES POWER LEVELS PERSISTED ON REBOOT.");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : VALUES MUST BE PERSISTED ON REBOOT.");
	    errorMessage = "UNABLE TO REBOOT THE DEVICE.";
	    if (isRebooted) {
		wifiAccessPoints = BroadBandMgmtFramePwrControlUtils.getWifiAccessPointsBasedOnRadio(true);
		resultObj = BroadBandMgmtFramePwrControlUtils.verifyMgmtPowerLevels(tapEnv, device, wifiAccessPoints,
			false);
		result = resultObj.isStatus();
		errorMessage = "UNABLE TO VERIFY THE 5GHz WIFI ACCESS POINT MANAGEMENT FRAMES POWER LEVELS PERSIST ON REBOOT: "
			+ resultObj.getErrorMessage();
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	    } else {
		tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_TESTED, errorMessage,
			false);
	    }
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : " + (result
		    ? "VERIFIED THE VALUES OF MANAGEMENT FRAME POWER LEVEL FOR 5GHz WIFI ACCESS POINTS PERSIST ON REBOOT."
		    : errorMessage));
	    LOGGER.info("#######################################################################################");

	    /**
	     * S21) Verify the 2.4 GHz WiFi Access Point's Management Frames power values are persisted after image
	     * upgrade.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": VERIFY 2.4GHz WIFI ACCESS POINT MANAGEMENT FRAMES POWER LEVELS PERSISTED ON IMAGE UPGRADE.");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : VALUES MUST BE PERSISTED ON IMAGE UPGRADE.");
	    currentFirmwareVersion = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
	    LOGGER.info("CURRENT FIRMWARE VERSION: " + currentFirmwareVersion);
	    errorMessage = "UNABLE TO TRIGGER THE IMAGE UPGRADE ON THE DEVICE.";
	    try {
		hasBuildChanged = FirmwareDownloadUtils.getLatestAvailableImageAndTriggerCdl(tapEnv, device,
			currentFirmwareVersion);
		// In case the HTTP TR-181 CDL Fails, then trigger CDL using TFTP Docsis SNMP
		// Command.
		if (!hasBuildChanged) {
		    LOGGER.error("HTTP TR-181 CDL FAILED; HENCE GOING TO TRIGGER CDL WITH TFTP DOCSIS SNMP COMMANDS.");
		    hasBuildChanged = FirmwareDownloadUtils.triggerAndWaitForTftpCodeDownloadUsingDocsisSnmpCommand(
			    tapEnv, device, tapEnv.getLatestBuildImageVersionForCdlTrigger(device, false)
				    + BroadBandCdlConstants.BIN_EXTENSION,
			    false);

		}
	    } catch (TestException testException) {
		// Log & Suppress the Exception
		LOGGER.error(testException.getMessage());
	    }

	    if (hasBuildChanged) {
		wifiAccessPoints = BroadBandMgmtFramePwrControlUtils.getWifiAccessPointsBasedOnRadio(false);
		resultObj = BroadBandMgmtFramePwrControlUtils.verifyMgmtPowerLevels(tapEnv, device, wifiAccessPoints,
			false);
		result = resultObj.isStatus();
		errorMessage = "UNABLE TO VERIFY THE 2.4GHz WIFI ACCESS POINT MANAGEMENT FRAMES POWER LEVELS PERSIST ON IMAGE UPGRADE: "
			+ resultObj.getErrorMessage();
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	    } else {
		tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_TESTED, errorMessage,
			false);
	    }
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : " + (result
		    ? "VERIFIED THE VALUES OF MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz WIFI ACCESS POINTS PERSIST ON IMAGE UPGRADE."
		    : errorMessage));
	    LOGGER.info("#######################################################################################");

	    /**
	     * S22) Verify the 5 GHz WiFi Access Point's Management Frames power values are persisted after image
	     * upgrade.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": VERIFY 5GHz WIFI ACCESS POINT MANAGEMENT FRAMES POWER LEVELS PERSISTED ON IMAGE UPGRADE.");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : VALUES MUST BE PERSISTED ON IMAGE UPGRADE.");
	    errorMessage = "UNABLE TO PERFORM IMAGE UPGRADE ON THE DEVICE.";
	    if (hasBuildChanged) {
		wifiAccessPoints = BroadBandMgmtFramePwrControlUtils.getWifiAccessPointsBasedOnRadio(true);
		resultObj = BroadBandMgmtFramePwrControlUtils.verifyMgmtPowerLevels(tapEnv, device, wifiAccessPoints,
			false);
		result = resultObj.isStatus();
		errorMessage = "UNABLE TO VERIFY THE 5GHz WIFI ACCESS POINT MANAGEMENT FRAMES POWER LEVELS PERSIST ON IMAGE UPGRADE: "
			+ resultObj.getErrorMessage();
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	    } else {
		tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_TESTED, errorMessage,
			false);
	    }
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : " + (result
		    ? "VERIFIED THE VALUES OF MANAGEMENT FRAME POWER LEVEL FOR 5GHz WIFI ACCESS POINTS PERSIST ON IMAGE UPGRADE."
		    : errorMessage));
	    LOGGER.info("#######################################################################################");

	    /**
	     * S23) Verify the 2.4 GHz WiFi Access Point's Management Frames power values are set to 0 on performing
	     * Factory Reset.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": VERIFY 2.4GHz WIFI ACCESS POINT MANAGEMENT FRAMES POWER LEVELS RESET TO 0 ON FACTORY RESET.");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : VALUES MUST BE RESET TO 0.");
	    errorMessage = "MARKING THIS STEP AS NOT TESTED AS FACTORY RESET COULD NOT BE PERFORMED ON THE DEVICE.";
	    hasFactoryReset = BroadBandCommonUtils.performFactoryResetSnmp(tapEnv, device);
	    if (!hasFactoryReset) {
		hasFactoryReset = BroadBandCommonUtils.performFactoryResetWebPa(tapEnv, device);
	    }
	    if (hasFactoryReset) {
		errorMessage = "WEBPA PROCESS IS NOT COMING UP. HENCE BLOCKING THE EXECUTION.";
		result = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
		LOGGER.info("WEBPA PROCESS IS UP & RUNNING: " + result);
	    }
	    if (result) {
		wifiAccessPoints = BroadBandMgmtFramePwrControlUtils.getWifiAccessPointsBasedOnRadio(false);
		resultObj = BroadBandMgmtFramePwrControlUtils.verifyMgmtPowerLevels(tapEnv, device, wifiAccessPoints,
			true);
		result = resultObj.isStatus();
		errorMessage = "UNABLE TO VERIFY THE 2.4GHz WIFI ACCESS POINT MANAGEMENT FRAMES POWER LEVELS RESET TO 0 ON FACTORY RESET: "
			+ resultObj.getErrorMessage();
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	    } else {
		tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_TESTED, errorMessage,
			false);
	    }
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : " + (result
		    ? "VERIFIED THE VALUES OF MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz WIFI ACCESS POINTS RESET TO 0 ON FACTORY RESET."
		    : errorMessage));

	    /**
	     * S24) Verify the 5 GHz WiFi Access Point's Management Frames power values are set to 0 on performing
	     * Factory Reset.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": VERIFY 5GHz WIFI ACCESS POINT MANAGEMENT FRAMES POWER LEVELS RESET TO 0 ON FACTORY RESET.");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : VALUES MUST BE RESET TO 0.");
	    errorMessage = "MARKING THIS STEP AS NOT TESTED AS FACTORY RESET COULD NOT BE PERFORMED ON THE DEVICE.";
	    if (hasFactoryReset) {
		wifiAccessPoints = BroadBandMgmtFramePwrControlUtils.getWifiAccessPointsBasedOnRadio(false);
		resultObj = BroadBandMgmtFramePwrControlUtils.verifyMgmtPowerLevels(tapEnv, device, wifiAccessPoints,
			true);
		result = resultObj.isStatus();
		errorMessage = "UNABLE TO VERIFY THE 5GHz WIFI ACCESS POINT MANAGEMENT FRAMES POWER LEVELS RESET TO 0 ON FACTORY RESET: "
			+ resultObj.getErrorMessage();
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	    } else {
		tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_TESTED, errorMessage,
			false);
	    }
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : " + (result
		    ? "VERIFIED THE VALUES OF MANAGEMENT FRAME POWER LEVEL FOR 5GHz WIFI ACCESS POINTS RESET TO 0 ON FACTORY RESET."
		    : errorMessage));
	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("EXCEPTION OCCURRED WHILE VALIDATING THE MANAGEMENT FRAME POWER CONTROL USING WEBPA: "
		    + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, result, errorMessage, true);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    int postConditionStepNum = 0;

	    result = false;
	    String imageAfterTriggering = "";
	    errorMessage = "Device is not accessible even after waiting for 10 mins.";
	    String successMessage = "";
	    postConditionStepNum++;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("POST-CONDITION " + postConditionStepNum
		    + ": DESCRIPTION : Verify reverting device to original image.");
	    LOGGER.info("POST-CONDITION " + postConditionStepNum
		    + ": ACTION : Flash the original build on the device using HTTP/ TR-181.");
	    LOGGER.info("POST-CONDITION " + postConditionStepNum
		    + ": EXPECTED : Device should be upgraded to original image.");
	    LOGGER.info("**********************************************************************************");
	    try {
		if (CommonMethods.isSTBAccessible(device)) {
		    errorMessage = "Unable to get current firmware version.";
		    imageAfterTriggering = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
		    if (CommonMethods.isNotNull(imageAfterTriggering)
			    && CommonMethods.isNotNull(currentFirmwareVersion)) {
			if (!imageAfterTriggering.equals(currentFirmwareVersion)) {
			    // result =
			    // BroadBandCodeDownloadUtils.upgradeDeviceWithGivenFirmwareVersion(device,
			    // tapEnv,
			    // currentFirmwareVersion);
			    // if (!result) {
			    result = BroadBandCodeDownloadUtils.triggerPreviousCodeDownload(device, tapEnv,
				    currentFirmwareVersion);
			    // }
			} else {
			    successMessage = "Device Build hasn't changed so need to revert the device image.";
			    result = true;
			}
		    }
		}
	    } catch (Exception e) {
		errorMessage = "Exception occured during reverting the device back to original image." + errorMessage
			+ e.getMessage();
	    }
	    if (result) {
		LOGGER.info("POST-CONDITION " + postConditionStepNum + ": ACTUAL : " + successMessage);
	    } else {
		LOGGER.error("POST-CONDITION " + postConditionStepNum + ": ACTUAL : " + errorMessage);
	    }

	    if (hasBuildChanged) {
		result = false;
		errorMessage = "Unable to perform device reactivation.";
		postConditionStepNum++;
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"POST-CONDITION " + postConditionStepNum + ": DESCRIPTION : Verify reactivating the device.");
		LOGGER.info(
			"POST-CONDITION " + postConditionStepNum + ": ACTION : Reactivate device using SNMP or Webpa.");
		LOGGER.info("POST-CONDITION " + postConditionStepNum
			+ ": EXPECTED : Device reactivation should be successful.");
		LOGGER.info("**********************************************************************************");
		try {
		    result = BroadBandWiFiUtils.reactivateDeviceUsingWebpaOrSnmp(tapEnv, device);
		} catch (Exception e) {
		    errorMessage = "Exception occured during reactivating the device." + errorMessage + e.getMessage();
		}
		if (result) {
		    LOGGER.info("POST-CONDITION " + postConditionStepNum
			    + ": ACTUAL : Device reactivation performed succesfully.");
		} else {
		    LOGGER.error("POST-CONDITION " + postConditionStepNum + ": ACTUAL : " + errorMessage);
		}
	    }
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("COMPLETED TEST CASE: TC-RDKB-WIFI-5051");
    }

    /**
     * <p>
     * Steps:
     * </p>
     * <ol>
     * <li>STEP 1: Disable 2.4GHz radio using WebPA command.</li>
     * <li>STEP 2: Verify enabling private SSID of 2.4 Ghz using from MSO page, SMNP, TR069 and WEBPA.</li>
     * <li>STEP 3: Disable 5GHz radio using WebPA command.</li>
     * <li>STEP 4: Verify enabling private SSID of 5 Ghz using from MSO page, SMNP, TR069 and WEBPA.</li>
     * <li>STEP 5: Reboot the device.</li>
     * <li>STEP 6: Verify 2.4 Ghz Radio value.</li>
     * <li>STEP 7: Verify 5 Ghz Radio value.</li>
     * </ol>
     * 
     * @param device
     *            instance of {@link Dut}
     * @author Praveenkumar Paneerselvam
     * @Refactor Rakesh C N, Sruthi Santhosh
     */
    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
	    BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-WIFI_RADIO-1002")
    public void testToVerifyAbilityToDisableWiFiRadioFromWebpa(Dut device) {
	// boolean variable to store the status
	boolean status = false;
	// Test case id
	String testId = "TC-RDKB-WIFI_RADIO-002";
	// Test step number
	String testStepNumber = "s1";
	// error message
	String errorMessage = null;
	// Pre set value
	List<String> radioStatusValue = null;
	// Radio Status webpa parameters of 2.4 Ghz and 5 Ghz
	String[] webpaParameters = { BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_RADIO_ENABLE,
		BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_RADIO_ENABLE };
	// String pod = null;
	BroadBandResultObject executionStatus = null;

	try {
	    radioStatusValue = executePreConditionToGetRadioStatusUsingWebpa(device, webpaParameters);
	    /**
	     * 
	     * /** Step 1 :Disable 2.4GHz radio using WebPA command.
	     */
	    LOGGER.info("**********************************************");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI_RADIO-1002");
	    LOGGER.info("STEP 1: DESCRIPTION : Disable 2.4GHz radio using WebPA command. ");
	    LOGGER.info("STEP 1:  ACTION : Execute below command. \n"
		    + "curl -i -H \"Authorization:Bearer <SAT_TOKEN>\" -X PATCH -H \"Content-Type:application/json\" -H \"Accept:application/json\" -w %{time_total} -k "
		    + "\"<WEBPA URL>/api/v2/device/mac:<ECM_MAC>/config\" -d '{\"parameters\":[{\"dataType\":3,\"name\":\"Device.WiFi.SSID.10001.Enable\",\"value\":\"false\"}]}' ");
	    LOGGER.info("STEP 1: EXPECTED :2.4GHz radio should be disabled successfully. ");
	    LOGGER.info("**********************************************");
	    errorMessage = BroadBandCommonUtils.concatStringUsingStringBuffer("Failed to set webpa parameter ",
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_RADIO_ENABLE, ERR_MSG_WITH_VALUE,
		    BroadBandTestConstants.FALSE);
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_RADIO_ENABLE, WebPaDataTypes.BOOLEAN.getValue(),
		    BroadBandTestConstants.FALSE);
	    LOGGER.info("STEP 1 : ACTUAL: " + (status ? "Successfully disabled 2.4 Ghz wifi radio" : errorMessage));
	    // updateExecutionStatus(device, testId, testStepNumber, status, errorMessage,
	    // false);

	    /**
	     * Step 2 :Verify enabling private SSID of 2.4 Ghz using from MSO page, SMNP, TR069 and WEBPA.
	     */
	    LOGGER.info("**********************************************");
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Verify enabling private SSID of 2.4 Ghz using from MSO page, SMNP, TR069 and WEBPA. ");
	    LOGGER.info("STEP 2:  ACTION : Execute below steps to verify \n");
	    LOGGER.info("STEP 2: EXPECTED : Private SSID of 2.4 Ghz shouldn't be allowed to enable .. ");
	    LOGGER.info("**********************************************");
	    testStepNumber = "s2";
	    if (status) {
		status = false;

		executionStatus = verifyEnablingPrivateSsid(device, SSIDFrequency.FREQUENCY_2_4_GHZ);
		status = executionStatus.isStatus();
		errorMessage = executionStatus.getErrorMessage();
		if (status) {
		    LOGGER.info("STEP 2: ACTUAL :Private SSID of 2.4 Ghz is not editable");
		} else {
		    LOGGER.error("STEP 2 : ACTUAL : " + errorMessage);
		}
		LOGGER.info("*************************************************************************");

		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
	    } else {
		errorMessage = "RADIO IS DISABLED HENCE ACTION THROUGH WEBPA TO ENABLE THE INDIVIDUAL SSID OF 2.4 GHZ SHOULDN'T WORK.MARKED STEP NO 2 AS NOT TESTED.";
		LOGGER.error("STEP 2 :ACTUAL : " + errorMessage);
		tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_TESTED,
			errorMessage, false);
	    }

	    /**
	     * Step 3 :Disable 5GHz radio using WebPA command..
	     */
	    LOGGER.info("**********************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Disable 5GHz radio using WebPA command. ");
	    LOGGER.info("STEP 3:  ACTION : Execute below command. \n"
		    + "curl -i -H \"Authorization:Bearer <SAT_TOKEN>\" -X PATCH -H \"Content-Type:application/json\" -H \"Accept:application/json\" -w %{time_total} -k "
		    + "\"<WEBPA URL>/api/v2/device/mac:<ECM_MAC>/config\" -d '{\"parameters\":[{\"dataType\":3,\"name\":\"Device.WiFi.SSID.10101.Enable\",\"value\":\"false\"}]}' ");
	    LOGGER.info("STEP 3: EXPECTED :5GHz radio should be disabled successfully. ");
	    status = false;
	    testStepNumber = "s3";
	    errorMessage = BroadBandCommonUtils.concatStringUsingStringBuffer("Failed to set webpa parameter ",
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_RADIO_ENABLE, ERR_MSG_WITH_VALUE,
		    BroadBandTestConstants.FALSE);
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_RADIO_ENABLE, WebPaDataTypes.BOOLEAN.getValue(),
		    BroadBandTestConstants.FALSE);
	    LOGGER.info("STEP 3 : ACTUAL: " + (status ? "Successfully disabled 5 Ghz wifi radio" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 4 :Verify enabling private SSID of 5 Ghz using from MSO page, SMNP, TR069 and WEBPA.
	     */
	    LOGGER.info("**********************************************");
	    LOGGER.info(
		    "STEP 4: DESCRIPTION : Verify enabling private SSID of 5Ghz using from MSO page, SMNP, TR069 and WEBPA. ");
	    LOGGER.info("STEP 4:  ACTION : Execute below steps to verify \n");
	    LOGGER.info("STEP 4: EXPECTED : Private SSID of 5 Ghz shouldn't be allowed to enable. ");
	    LOGGER.info("**********************************************");
	    testStepNumber = "s4";
	    if (status) {

		status = false;

		executionStatus = verifyEnablingPrivateSsid(device, SSIDFrequency.FREQUENCY_5_GHZ);
		status = executionStatus.isStatus();
		errorMessage = executionStatus.getErrorMessage();
		if (status) {
		    LOGGER.info("STEP 4: ACTUAL :Private SSID of 5 Ghz is not editable");
		} else {
		    LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
		}
		LOGGER.info("*************************************************************************");

		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
	    } else {
		errorMessage = "RADIO IS DISABLED HENCE ACTION THROUGH WEBPA TO ENABLE THE INDIVIDUAL SSID OF 2.4 GHZ SHOULDN'T WORK.MARKED STEP NO 4 AS NOT TESTED.";
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
		tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_TESTED,
			errorMessage, false);
	    }
	    /**
	     * Step 5 :Reboot the device.
	     */
	    LOGGER.info("**********************************************");
	    LOGGER.info("STEP 5: DESCRIPTION : Reboot the device. ");
	    LOGGER.info("STEP 5:  ACTION : Execute the command : /sbin/reboot \n" +

		    "Note - " + "Device should reboot and wait for IP Acquisition. \n");
	    LOGGER.info("STEP 5: EXPECTED : Device rebooted and IP acquired successfully. ");
	    LOGGER.info("**********************************************");
	    status = false;
	    testStepNumber = "s5";
	    status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
	    errorMessage = "Failed to reboot the device ";
	    LOGGER.info("STEP 5 : ACTUAL: " + (status ? "Device rebooted and IP acquired successfully" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
	    // Step 6 and Step 7
	    verifyRadioStatusAfterReboot(device, testId, BroadBandTestConstants.CONSTANT_5,
		    BroadBandTestConstants.FALSE);
	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error(errorMessage);
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, false, errorMessage, true);
	} finally {
	    // POST CONDITION
	    executePostConditionToSetRadioStatusUsingWebpa(device, radioStatusValue, webpaParameters);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI_RADIO-1002");
    }

    /**
     * Method to get radio status of 2.4 Ghz and 5 Ghz
     * 
     * @param device
     *            Dut Instance
     * @param webpaParameters
     *            webpa parameter of radio status of 2.4 Ghz and 5 Ghz
     * @return list of radio status value of 2.4 Ghz and 5 Ghz
     * @author Pradeep Panneerselvam
     */
    private List<String> executePreConditionToGetRadioStatusUsingWebpa(Dut device, String[] webpaParameters) {
	LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	LOGGER.info("PRE-CONDITION STEPS");
	List<String> radioStatusValue = new ArrayList<String>();
	for (String webpaParameter : webpaParameters) {
	    LOGGER.info("PRE-CONDITION : DESCRIPTION : Get radio Status of " + webpaParameter);
	    LOGGER.info("PRE-CONDITION : ACTION : Execute webpa get command for the parameter " + webpaParameter);
	    LOGGER.info("PRE-CONDITION : EXPECTED : Value for " + webpaParameter + " should be obtained successfully ");
	    String radioStatus = tapEnv.executeWebPaCommand(device, webpaParameter);
	    LOGGER.info("Radio Status of " + webpaParameter + " is -" + radioStatus);
	    radioStatusValue.add(radioStatus);
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	}
	LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
	return radioStatusValue;
    }

    /**
     * Method to verify Radio Status After Reboot
     * 
     * @param device
     *            Dut Instance
     * @param testId
     *            testId to be tested
     * @param stepNumber
     *            stepNumber as per test plan
     * @param expectedValue
     *            value of radio status expected after reboot
     * @author Praveenkumar Paneerselvam
     * 
     */
    private void verifyRadioStatusAfterReboot(Dut device, String testId, int stepNumber, String expectedValue) {
	LOGGER.debug("STARTING METHOD: verifyRadioStatusAfterReboot");
	SSIDFrequency[] ssidFrequencies = SSIDFrequency.values();
	String radioStatus = null;
	String errorMessage = null;
	boolean status = false;
	String testStepNumber = null;
	for (SSIDFrequency ssidFrequency : ssidFrequencies) {
	    if (!ssidFrequency.getValue().equals(BroadBandTestConstants.SSID_FREQ_BOTH_BAND)) {

		LOGGER.info("**********************************************");
		LOGGER.info("STEP " + (++stepNumber) + ": DESCRIPTION : Verify " + ssidFrequency + "Ghz Radio value. ");
		LOGGER.info("STEP " + (stepNumber) + ":  ACTION : Execute below COMMAND to verify Radio status");
		LOGGER.info("STEP " + (stepNumber) + ": EXPECTED : Radio value for " + ssidFrequency
			+ "Ghz should be disabled. ");
		LOGGER.info("**********************************************");
		testStepNumber = "s" + (stepNumber);
		radioStatus = BroadBandCommonUtils.getRadioStatus(ssidFrequency, tapEnv, device);
		errorMessage = "Radio Status of " + ssidFrequency.toString()
			+ " is not persist after reboot. Expected Value -" + expectedValue + ".Actual Value -"
			+ radioStatus + ".";
		status = CommonMethods.isNotNull(radioStatus) && expectedValue.equalsIgnoreCase(radioStatus);
		LOGGER.info("STEP " + (stepNumber) + ": ACTUAL: "
			+ (status ? "Radio value for " + ssidFrequency + "Ghz is disabled. " : errorMessage));
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
		LOGGER.debug("ENDING METHOD: verifyRadioStatusAfterReboot");
	    }
	}
    }

    /**
     * Helper method to execute Post Condition to set the value on device
     * 
     * @param device
     *            Dut instance
     * @param radioStatusValue
     *            pre set Radio status value of 2.4 Ghz and 5 Ghz
     * @param webpaParameters
     *            webpa parameter of radio status of 2.4 Ghz and 5 Ghz
     * @author Praveenkumar Paneerselvam
     */
    private void executePostConditionToSetRadioStatusUsingWebpa(Dut device, List<String> radioStatusValue,
	    String[] webpaParameters) {
	LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	int indexValue = BroadBandTestConstants.CONSTANT_0;
	String radioStatus = null;
	for (String webpaParameter : webpaParameters) {
	    LOGGER.info("PRE-CONDITION : DESCRIPTION : Set radio Status of " + webpaParameter);
	    LOGGER.info("PRE-CONDITION : ACTION : Execute webpa set command for the parameter " + webpaParameter);
	    LOGGER.info("PRE-CONDITION : EXPECTED : Value for " + webpaParameter + " should be set successfully ");
	    radioStatus = radioStatusValue.get(indexValue);
	    boolean status = CommonMethods.isNotNull(radioStatus)
		    && BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv, webpaParameter,
			    WebPaDataTypes.BOOLEAN.getValue(), radioStatus);
	    LOGGER.info("Radio Status of " + webpaParameter + " Ghz is set successfully-" + status);
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    indexValue++;
	}
	LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
    }

    /**
     * Method to verify Private SSID enabling through SNMP/TR-069/WebPA
     * 
     * @param device
     *            Dut Instance
     * @param ssidFrequency
     *            Ssid Frequency for which value should be set.
     * @param password
     *            password of the day for the device
     * @return BroadbandResultObject with status and errorMessage, status will be true if all private ssid is not
     *         enabled through MSO/SNMP/TR-069/WebPA
     * @author Praveenkumar Paneerselvam
     */
    public BroadBandResultObject verifyEnablingPrivateSsid(Dut device, SSIDFrequency ssidFrequency) {
	LOGGER.debug("STARTING METHOD: verifyEnablingPrivateSsid");
	BroadBandResultObject executionStatus = new BroadBandResultObject();
	StringBuffer errorMessage = new StringBuffer();
	errorMessage.append("Able to modify private SSID of " + ssidFrequency.getValue() + "from - ");

	// 1.Through SNMP
	LOGGER.info("Validating through SNMP.");
	boolean snmpStatus = false;
	snmpStatus = !(BroadBandWiFiUtils.verifyEnablingPrivateSsidUsingSnmp(device, tapEnv, ssidFrequency));
	if (!snmpStatus) {
	    errorMessage.append(TestCategory.SNMP);
	    errorMessage.append(BroadBandTestConstants.SINGLE_SPACE_CHARACTER);
	}
	LOGGER.info("Status of enable Private SSID through SNMP is failed-" + snmpStatus);

	// 2.Through TR069
	LOGGER.info("Validating through TR069.");
	boolean tr69Status;
	if (!DeviceModeHandler.isBusinessClassDevice(device)) {
	    tr69Status = !(BroadBandWiFiUtils.verifyEnablingPrivateSsidUsingTr69(device, tapEnv, ssidFrequency));
	    if (!tr69Status) {
		errorMessage.append(TestCategory.TR69);
		errorMessage.append(BroadBandTestConstants.SINGLE_SPACE_CHARACTER);
	    }
	    LOGGER.info("Status of enable Private SSID through TR-69 is failed-" + tr69Status);
	} else {
	    tr69Status = true;
	    // NA
	    LOGGER.info("Status of enable Private SSID through TR-69 is NA for Business Class Devices" + tr69Status);
	}

	// 3.Through WEBPA
	LOGGER.info("Validating through WEBPA.");
	boolean webpaStatus = !(BroadBandWiFiUtils.verifyEnablingPrivateSsidUsingWebpa(device, tapEnv, ssidFrequency));
	if (!webpaStatus) {
	    errorMessage.append(TestCategory.WEBPA);
	}
	LOGGER.info("Status of enable Private SSID through WEBPA is failed-" + webpaStatus);

	LOGGER.info("ERROR MESSAGE - " + errorMessage.toString());
	executionStatus.setStatus(/* msoStatus && */snmpStatus && tr69Status && webpaStatus);
	executionStatus.setErrorMessage(errorMessage.toString());

	LOGGER.info("Status of enabling Private SSID through MSO/SNMP/TR-069/WEBPA is failed-"
		+ executionStatus.isStatus());
	LOGGER.debug("ENDING METHOD: verifyEnablingPrivateSsid");
	return executionStatus;
    }

    /**
     * Helper method to verify enabling private SSID from WebPA.
     * 
     * @param device
     *            Dut instance
     * @param tapEnv
     *            AutomaticsTapApi instance
     * @param ssidFrequency
     *            Wifi SSID frequency
     * @return true if private ssid is enabled
     */
    public static boolean verifyEnablingPrivateSsidUsingWebpa(Dut device, AutomaticsTapApi tapEnv,
	    SSIDFrequency ssidFrequency) {
	LOGGER.debug("STARTING METHOD : verifyEnablingPrivateSsidUsingWebpa");
	boolean status = false;

	// This parameter gives the value of the Private SSID in MSO page, ready only
	// parameter
	String checkSsidStatus = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_2_4GHZ_PRIVATE_SSID;
	// This parameter is for modifying Private SSID state.
	String enableSsid = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLE;
	if (SSIDFrequency.FREQUENCY_5_GHZ.equals(ssidFrequency)) {
	    checkSsidStatus = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_5GHZ_PRIVATE_SSID;
	    enableSsid = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS;
	}

	status = BroadBandWiFiUtils.setWebPaParams(device, enableSsid, BroadBandTestConstants.TRUE,
		WebPaDataTypes.BOOLEAN.getValue());

	LOGGER.info("Response from WEBPA command is -" + status);
	if (status) {
	    status = false;
	    String getResponse = tapEnv.executeWebPaCommand(device, checkSsidStatus);
	    LOGGER.info("Response from WEBPA GET command is -" + getResponse + ".");
	    status = CommonMethods.isNotNull(getResponse)
		    && !(BroadBandConnectedClientTestConstants.RADIO_STATUS_DOWN.equalsIgnoreCase(getResponse.trim()));
	}
	LOGGER.info("Is Private SSID enabled through WebPA- " + status);
	LOGGER.debug("ENDING METHOD : verifyEnablingPrivateSsidUsingWebpa");
	return status;
    }

    /**
     * Test to verify that WIFI radio does not support channel number other than the possible channels defined
     * 
     * <li>1. Get the possible channels for 2.4 GHz radio via webpa</li>
     * <li>2. Validate if the channels are within pre-defined list</li>
     * <li>3. Get the possible channels for 5 GHz radio via webpa</li>
     * <li>4. Validate if the channels are within pre-defined list</li>
     * 
     * @author Sathurya_R
     * @Refactor Alan_Bivera
     * 
     * @param device
     */

    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
	    BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-WIFI-POSSIBLE-CHANNELS-1001")

    public void testToValidateWifiRadioChannelsAreFromDefinedList(Dut device) {
	// Variable Declaration begins
	String testCaseId = "";
	String stepNum = "";
	String errorMessage = "";
	int stepNumber = 1;
	boolean status = false;
	String channelsSupported24 = "";
	String channelsSupported5 = "";
	// Variable Declaration Ends
	testCaseId = "TC-RDKB-WIFI-POSSIBLE-CHANNELS-001";
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-POSSIBLE-CHANNELS-1001");
	LOGGER.info(
		"TEST DESCRIPTION: Test to verify that WIFI radio does not support channel number other than the possible channels defined");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Get the possible channels for 2.4 GHz radio via webpa ");
	LOGGER.info("2. Validate if the channels are within pre-defined list ");
	LOGGER.info("3. Get the possible channels for 5 GHz radio via webpa ");
	LOGGER.info("4. Validate if the channels are within pre-defined list");
	LOGGER.info("#######################################################################################");
	try {
	    /**
	     * STEP 1 : GET THE POSSIBLE CHANNELS FOR 2.4 GHZ RADIO VIA WEBPA
	     */
	    stepNum = "S" + stepNumber;
	    status = false;
	    errorMessage = "Attempt to get possible channels via webpa for 2.4 GHz has failed";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP " + stepNumber + " :DESCRIPTION : Get the possible channels for 2.4 GHz radio via webpa ");
	    LOGGER.info("STEP " + stepNumber + " :ACTION : Execute webpa get on the parameter "
		    + BroadBandWebPaConstants.WEBPA_PARAM_FOR_POSSIBLECHANNELS_IN_2GHZ);
	    LOGGER.info("STEP " + stepNumber + " : EXPECTED : The webpa get should be successful ");
	    LOGGER.info("**********************************************************************************");

	    channelsSupported24 = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_POSSIBLECHANNELS_IN_2GHZ);
	    status = CommonMethods.isNotNull(channelsSupported24) && CommonMethods.patternMatcher(channelsSupported24,
		    (CommonMethods.isAtomSyncAvailable(device, tapEnv))
			    ? BroadBandTestConstants.PATTERN_MATCHER_24_GHZ_CHANNEL_ATOM
			    : BroadBandTestConstants.PATTERN_MATCHER_CHANNEL_LIST_WITH_COMMA);
	    if (status) {
		LOGGER.info("STEP " + stepNum + ": ACTUAL : Possible channels are obtained successfully via webpa");
	    } else {
		LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    /**
	     * STEP 2 : VALIDATE IF THE CHANNELS ARE WITHING PRE-DEFINED LIST
	     */
	    ++stepNumber;
	    stepNum = "S" + stepNumber;
	    status = false;
	    errorMessage = "The possible channels are not within pre-defined list";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + " :DESCRIPTION : Validate if the channels are within pre-defined list ");
	    LOGGER.info("STEP " + stepNumber + " :ACTION : Validate the channels against pre-defined list of 2.4 GHz");
	    LOGGER.info("STEP " + stepNumber + " : EXPECTED : The channels for 2.4 should be within expected list ");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWiFiUtils.validateIfChannelsAreFromSupportedList(channelsSupported24,
		    WiFiFrequencyBand.WIFI_BAND_2_GHZ, device);
	    if (status) {
		LOGGER.info("STEP " + stepNum + ": ACTUAL : Possible channels are withing the expected range");
	    } else {
		LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * STEP 3 : GET THE POSSIBLE CHANNELS FOR 5 GHZ RADIO VIA WEBPA
	     */
	    ++stepNumber;
	    stepNum = "S" + stepNumber;
	    status = false;
	    errorMessage = "Attempt to get possible channels via webpa for 5 GHz has failed";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + " :DESCRIPTION : Get the possible channels for 5 GHz radio via webpa ");
	    LOGGER.info("STEP " + stepNumber + " :ACTION : Execute webpa get on the parameter "
		    + BroadBandWebPaConstants.WEBPA_PARAM_FOR_POSSIBLECHANNELS_IN_5GHZ);
	    LOGGER.info("STEP " + stepNumber + " : EXPECTED : The webpa get should be successful ");
	    LOGGER.info("**********************************************************************************");

	    channelsSupported5 = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_POSSIBLECHANNELS_IN_5GHZ);
	    status = CommonMethods.isNotNull(channelsSupported5) && CommonMethods.patternMatcher(channelsSupported5,
		    BroadBandTestConstants.PATTERN_MATCHER_CHANNEL_LIST_WITH_COMMA);
	    if (status) {
		LOGGER.info("STEP " + stepNum + ": ACTUAL : Possible channels are obtained successfully via webpa");
	    } else {
		LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    /**
	     * STEP 4 : VALIDATE IF THE CHANNELS ARE WITHIN PRE-DEFINED LIST
	     */
	    ++stepNumber;
	    stepNum = "S" + stepNumber;
	    status = false;
	    errorMessage = "The possible channels are not within pre-defined list";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + " :DESCRIPTION : Validate if the channels are within pre-defined list ");
	    LOGGER.info("STEP " + stepNumber + " :ACTION : Validate the channels against pre-defined list of 5 GHz");
	    LOGGER.info("STEP " + stepNumber + " : EXPECTED : The channels for 5 should be within expected list ");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWiFiUtils.validateIfChannelsAreFromSupportedList(channelsSupported5,
		    WiFiFrequencyBand.WIFI_BAND_5_GHZ, device);
	    if (status) {
		LOGGER.info("STEP " + stepNum + ": ACTUAL : Possible channels are withing the expected range");
	    } else {
		LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	} catch (Exception e) {
	    LOGGER.error(
		    "Exception occured while trying to verify that WIFI radio does not support channel number other than the possible channels defined",
		    e);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-POSSIBLE-CHANNELS-1001");
    }

    /**
     * @TestDetails To enable XDNS feature and Verify status after Reboot and Factory Reset the device.*
     *              <li>PRE-CONDITION-1 : Set and verify the Global DNS IPv4 value using webpa param
     *              'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'</li>
     *              <li>PRE-CONDITION-2 : Set and verify the Global DNS IPv6 using webpa param
     *              'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv6'</li>
     *              <li>PRE-CONDITION-3 : Verify 2.4GHz private SSID status on the Device and Enable using webpa param
     *              'Device.WiFi.SSID.10001.Enable'</li>
     *              <li>PRE-CONDITION-4 : Verify 5GHz private SSID status on the Device and Enable using webpa param
     *              'Device.WiFi.SSID.10101.Enable'</li>
     * 
     *              <li>PRE-CONDITION-5 : Re-Activate and verify the device by setting 2.4GHz and 5GHz SSID and
     *              Passphrase</li>
     *              <li>PRE-CONDITION 6 : Factory reset the device and Verify whether WebPA is Up and Running in the
     *              Device</li>
     * 
     *              STEP 1: Verify if XDNS feature is disabled by default using webpa param
     *              Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS
     *              <li>EXPECTED: XDNS Feature must be disabled
     *
     *              STEP 2: Enable the XDNS feature by setting true to webpa param
     *              Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS
     *              <li>EXPECTED: XDNS feature must enabled successfully
     *
     *              STEP 3: Reboot the broadband device and Verify if device is up
     *              <li>EXPECTED: Device should come up and the image version should be not null
     *
     *              STEP 4: Verify if XDNS feature persist Enable status after device reboot
     *              <li>EXPECTED: XDNS feature should persist Enable status after device reboot
     * 
     *              STEP 5 : Backup SecConsole log and PAMlog to nvram
     *              <li>EXPECTED: Backup of secconsole and pamlog is successful
     *
     *              STEP 6 : Factory Reset the device and verify if device comes up
     *              <li>EXPECTED: Device should come up after Factory reset
     * 
     *              STEP 7 : Validate for after reboot Device led logs in SecConsole.txt.0 and PAMlog.txt.0
     *              <li>EXPECTED: After reboot led logs should be caputured
     *
     *              STEP 8 : Validate before reboot Device led logs in Backup SecConsole.txt.0 and PAMlog.txt.0 In nvram
     *              <li>EXPECTED: Before factory reset led logs should be caputured
     * 
     *              STEP 9: Verify the Captive portal status after Factory reseting the router using webpa param
     *              Device.DeviceInfo.X_RDKCENTRAL-COM_CaptivePortalEnable
     *              <li>EXPECTED: Webpa param Device.DeviceInfo.X_RDKCENTRAL-COM_CaptivePortalEnable should return true
     *
     *              STEP 10: Verify if the XDNS feature is Disabled after factory reset using webpa param
     *              Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS
     *              <li>EXPECTED: XDNS Feature must be Disabled *
     *              <li>POST-CONDITION 1 : Disable and verify the XDNS feature using webpa param
     *              'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'</li>
     * 
     *              <li>POST-CONDITION 2 : Reactivate the router device after factory reset</li>
     *              <li>POST-CONDITION 3 : Delete temporary files in Nvram</li>
     * @param device
     * @author Susheela C,prasanthreddy.a
     * @Refactor Sruthi Santhosh
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    BroadBandTestGroup.XDNS })
    @TestDetails(testUID = "TC-RDKB-XDNS-1000")

    public void enableXDNSAndVerifyStatusAfterRebootAndFactoryReset(Dut device) {
	// Holds the test case ID
	String testCaseId = "TC-RDKB-XDNS-100";
	// boolean variable to store the status
	boolean status = false;
	// boolean variable to store the factory reset status
	boolean factoryResetStatus = false;
	// Test step number
	String stepNumber = "s1";
	// Error message
	String errorMessage = null;
	// String to hold the response
	String response = null;
	Map<String, String> backupMap = null;

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-XDNS-1000");
	LOGGER.info(
		"TEST DESCRIPTION: To  enable XDNS feature and Verify status after Reboot and Factory Reset the device");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info(
		"PRE-CONDITION-1 : Set and verify the Global DNS IPv4 value using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'");
	LOGGER.info(
		"PRE-CONDITION-2 : Set and verify the Global DNS IPv6 value using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv6'");
	LOGGER.info(
		"PRE-CONDITION-3 : Verify 2.4GHz private SSID status on the Device and Enable using webpa param 'Device.WiFi.SSID.10001.Enable'");
	LOGGER.info(
		"PRE-CONDITION-4 : Verify 5GHz private SSID status on the Device and Enable using webpa param 'Device.WiFi.SSID.10101.Enable'");

	LOGGER.info(
		"PRE-CONDITION-5 : Re-Activate and verify the device by setting 2.4GHz and 5GHz SSID and Passphrase");
	LOGGER.info(
		"PRE-CONDITION 6 : Factory reset the device and Verify whether WebPA is Up and Running in the Device");
	LOGGER.info(
		"STEP 1: Verify if XDNS feature is disabled by default using webpa param Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS");
	LOGGER.info(
		"STEP 2: Enable the XDNS feature by setting true to webpa param Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS");
	LOGGER.info("STEP 3: Reboot the broadband device and Verify if device is up");
	LOGGER.info("STEP 4: Verify if XDNS feature persist Enable status after device reboot");
	LOGGER.info("STEP 5 : Backup SecConsole log and PAMlog to nvram");
	LOGGER.info("STEP 6 : Factory Reset the device and verify if device comes up");
	LOGGER.info("STEP 7 : Validate for after reboot Device led logs in SecConsole.txt.0 and PAMlog.txt.0");
	LOGGER.info(
		"STEP 8 : Validate before reboot Device led logs in Backup SecConsole.txt.0 and PAMlog.txt.0 In nvram");
	LOGGER.info(
		"STEP 9: Verify the Captive portal status after Factory reseting the router using webpa param Device.DeviceInfo.X_RDKCENTRAL-COM_CaptivePortalEnable");
	LOGGER.info(
		"STEP 10: Verify if the XDNS feature is Disabled after factory reset using webpa param Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS");
	LOGGER.info(
		"POST-CONDITION 1  : Disable and verify the XDNS feature using webpa param 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'");

	LOGGER.info("POST-CONDITION 2  : Reactivate the router device after factory reset");
	LOGGER.info("POST-CONDITION 3  : Delete temporary files in Nvram");
	LOGGER.info("#######################################################################################");
	try {
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "PRE-CONDITION 1: DESCRIPTION : Factory reset the device and Verify whether WebPA is Up and Running in the Device");
	    LOGGER.info("PRE-CONDITION 1: ACTION : Performing factory reset");
	    LOGGER.info("PRE-CONDITION 1: EXPECTED : The device should get factory resetted and device should come up");
	    LOGGER.info("#####################################################################################");
	    if (!BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_XDNS_FEATURE_STATUS, BroadBandTestConstants.FALSE,
		    BroadBandTestConstants.THREE_MINUTES, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)) {
		status = BroadBandCommonUtils.performFactoryResetWebPaByPassingTriggerTime(tapEnv, device,
			BroadBandTestConstants.EIGHT_MINUTE_IN_MILLIS);
		if (status) {
		    LOGGER.info("PRE-CONDITION 1: ACTUAL: Factory Reset is successful");
		} else {
		    LOGGER.error("PRE-CONDITION 1: ACTUAL : Factory reset is not successfull");
		    throw new TestException(
			    BroadBandTestConstants.PRE_CONDITION_ERROR + "Factory Resetting the device failed");
		}
	    } else {
		LOGGER.info(
			"Skipping Pre condition 1 to factory reset the gateway since Xdns feature is already in default state(disabled state).");
	    }

	    status = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info(
		    "PRE-CONDITION 2: DESCRIPTION : Set and verify the Global DNS IPv4 value using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'");
	    LOGGER.info(
		    "PRE-CONDITION 2: ACTION : Set the Global DNS IPv4 value using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'");
	    LOGGER.info(
		    "PRE-CONDITION 2: EXPECTED : Global DNS IPv4 value should be set using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'");
	    LOGGER.info("#######################################################################################");

	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_XDNS_IPV4, WebPaDataTypes.STRING.getValue(),
		    AutomaticsTapApi.getSTBPropsValue(AutomaticsPropertyUtility
		    		.getProperty(BroadBandPropertyKeyConstants.STRING_DEFAULT_GLOBAL_DNS_IPV4_VALUE)),
		    AutomaticsConstants.THREE_MINUTES, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

	    // Error message
	    errorMessage = "Failed to set Global DNS IPv4 value using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'";
	    if (status) {
		LOGGER.info("PRE-CONDITION 2: ACTUAL: Global DNS IPv4 value sucessfully set");
	    } else {
		LOGGER.error("PRE-CONDITION 2: ACTUAL : " + errorMessage);
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 2 FAILED : " + errorMessage);
	    }

	    status = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info(
		    "PRE-CONDITION 3: DESCRIPTION : Set and verify the Global DNS IPv6 value using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv6'");
	    LOGGER.info(
		    "PRE-CONDITION 3: ACTION : Set the Global DNS IPv6 value using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv6'");
	    LOGGER.info(
		    "PRE-CONDITION 3: EXPECTED : Global DNS IPv6 value should be set using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv6'");
	    LOGGER.info("#######################################################################################");
	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_XDNS_IPV6, WebPaDataTypes.STRING.getValue(),
		    AutomaticsTapApi.getSTBPropsValue(BroadBandTestConstants.STRING_DEFAULT_GLOBAL_DNS_IPV6_VALUE),
		    AutomaticsConstants.THREE_MINUTES, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    // Error message
	    errorMessage = "Failed to set Global DNS IPv6 value using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv6'";
	    if (status) {
		LOGGER.info("PRE-CONDITION 3: ACTUAL: Global DNS IPv6 value sucessfully set'");
	    } else {
		LOGGER.error("PRE-CONDITION 3: ACTUAL : " + errorMessage);
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION-3 FAILED : " + errorMessage);
	    }
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

	    /**
	     * STEP 1: Verify if XDNS feature is disabled by default using webpa param
	     * Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS
	     * <li>EXPECTED: XDNS Feature must be disabled
	     */
	    // Test step number
	    stepNumber = "s1";
	    // boolean variable to store the status
	    status = false;
	    // Error message
	    errorMessage = "XDNS feature is not disabled by default, validated using tr181 param Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : DESCRIPTION : Verify if XDNS feature is disabled by default using webpa param Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : ACTION : Get XDNS feature status using webpa param Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS");
	    LOGGER.info("STEP :  " + stepNumber + " : EXPECTED: XDNS Feature must be disabled by default");
	    LOGGER.info("**********************************************************************************");
	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_XDNS_FEATURE_STATUS);
	    status = CommonMethods.isNotNull(response)
		    && CommonUtils.patternSearchFromTargetString(response, AutomaticsConstants.FALSE);
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : XDNS feature is disabled by default");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

	    /**
	     * STEP 2: Enable the XDNS feature by setting true to webpa param
	     * Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS
	     * <li>EXPECTED: XDNS feature must enabled successfully
	     */
	    stepNumber = "s2";
	    status = false;
	    errorMessage = "Unable to set new tag name to device by this parameter "
		    + BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_XDNS_DEVICE_TAG;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP :  " + stepNumber + " : DESCRIPTION : Enable XDNS with default settings");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : ACTION : Execute command :dmcli eRT setv Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceTag string Test_xdns1"
		    + "dmcli eRT getv Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceTag"
		    + "dmcli eRT setv Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS bool true"
		    + "dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS");
	    LOGGER.info("STEP :  " + stepNumber + " : EXPECTED : Check values get set and enabled properly");
	    LOGGER.info("**********************************************************************************");
	    if (BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_XDNS_DEVICE_TAG, BroadBandTestConstants.CONSTANT_0,
		    BroadBandTraceConstants.XDNS_TAG_NAME)) {
		errorMessage = "Not able to enable XDNS by using this parameter "
			+ BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_XDNS_FEATURE_STATUS;
		status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_XDNS_FEATURE_STATUS,
			BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);
	    }
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Successfully enabled XDNS with default settings.");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	    /**
	     * STEP 3: Reboot the broadband device and Verify if device is up
	     * <li>EXPECTED: Device should come up and the image version should be not null
	     */
	    // Test step number
	    stepNumber = "s3";
	    // boolean variable to store the status
	    status = false;
	    // Error message
	    errorMessage = "Device didn't come up after reboot";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : DESCRIPTION : Reboot the broadband device and Verify if device is up");
	    LOGGER.info("STEP :  " + stepNumber + " : ACTION : Execute '/sbin/reboot' command in the device");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : EXPECTED: Device should come up and the image version should be not null");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandCommonUtils.rebootViaWebpaAndWaitForStbAccessible(device, tapEnv)
		    && CommonMethods.waitForEstbIpAcquisition(tapEnv, device);
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : Device rebooted and came up successfully");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }

	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
	    /**
	     * STEP 4: Verify if XDNS feature persist Enable status after device reboot
	     * <li>EXPECTED: XDNS feature should persist Enable status after device reboot
	     */
	    // Test step number
	    stepNumber = "s4";
	    // boolean variable to store the status
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : DESCRIPTION : Verify if XDNS feature persist Enable status after device reboot");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : ACTION : Get XDNS feature status using webpa param Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : EXPECTED: XDNS feature should persist Enable status after device reboot");
	    LOGGER.info("**********************************************************************************");
	    // Error message
	    errorMessage = "Webpa service is not up in the device after reboot. Unable to verify XDNS feature status.";
	    if (BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true)) {
		// Error message
		errorMessage = "XDNS feature did not persist Enable status after device reboot";
		response = tapEnv.executeWebPaCommand(device,
			BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_XDNS_FEATURE_STATUS);
		if (CommonMethods.isNotNull(response)
			&& CommonUtils.patternSearchFromTargetString(response, AutomaticsConstants.TRUE)) {
		    status = true;
		}
	    }
	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : XDNS Feature is enabled after rebooting the device.");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

	    /** Step 5 : Backup Secconsole,Pamlog logs to given path */
	    backupMap = BroadBandSnmpTest.helperMethodToBackupFiles(device, tapEnv, testCaseId,
		    BroadBandTestConstants.CONSTANT_5);

	    /**
	     * STEP 6 : Factory Reset the device and verify if device comes up
	     * <li>EXPECTED: Device should come up after Factory reset
	     */
	    // Test step number
	    stepNumber = "s6";
	    // boolean variable to store the status
	    status = false;

	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : DESCRIPTION : Factory Reset the device and verify if device comes up");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : ACTION : Factory Reset the device using webpa param Device.X_CISCO_COM_DeviceControl.FactoryReset");
	    LOGGER.info("STEP :  " + stepNumber + " : EXPECTED: Device should come up after Factory reset");
	    LOGGER.info("#######################################################################################");
	    // Error message
	    errorMessage = "Failed to Tigger Factory Reset/Device didn't come up after Factory Reset.";

	    status = BroadBandCommonUtils.performFactoryResetWebPa(tapEnv, device);

	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : Device came up after Factory reset successfully.");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

	    /** Step 7 and Step 8 To validate LED Logs before and after FR */
	    BroadBandSnmpTest.helperMethodToValidateLedLogs(device, tapEnv, backupMap, testCaseId,
		    BroadBandTestConstants.CONSTANT_7);

	    /**
	     * STEP 9: Verify the Captive portal status after Factory reseting the router using webpa param
	     * Device.DeviceInfo.X_RDKCENTRAL-COM_CaptivePortalEnable
	     * <li>EXPECTED: Webpa param Device.DeviceInfo.X_RDKCENTRAL-COM_CaptivePortalEnable should return true
	     * 
	     */
	    // Test step number
	    stepNumber = "s9";
	    // boolean variable to store the factory reset status
	    factoryResetStatus = true;
	    // boolean variable to store the status
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : DESCRIPTION : Verify the Captive portal status after Factory reseting the router using webpa param Device.DeviceInfo.X_RDKCENTRAL-COM_CaptivePortalEnable");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : ACTION : Get Captive portal status using webpa param Device.DeviceInfo.X_RDKCENTRAL-COM_CaptivePortalEnable");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : EXPECTED: Webpa param Device.DeviceInfo.X_RDKCENTRAL-COM_CaptivePortalEnable should return true");
	    LOGGER.info("**********************************************************************************");
	    
	    String currentPartnerIdName = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
	    		BroadBandWebPaConstants.WEBPA_PARAM_FOR_SYNDICATION_PARTNER_ID);

	    if (!DeviceModeHandler.isDSLDevice(device)) {

		// Error message
		errorMessage = "Device is not in Captive portal after factory reset.";
		try {
		    factoryResetStatus = BroadBandWiFiUtils.verifyCaptivePortalModeUsingWebPaCommand(tapEnv, device);
		} catch (Exception exe) {
		    errorMessage = "Following Exception occurred while querying the Captive portal status using WebPA/Dmcli command  -> "
			    + exe.getMessage();
		}

		if (factoryResetStatus) {
		    LOGGER.info("STEP 9: ACTUAL : Device is in Captive portal after factory reset as excepted.");
		} else {
		    LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, factoryResetStatus, errorMessage, true);
	    } else if (BroadBandCommonUtils.verifySpecificSyndicationPartnerAvailability(currentPartnerIdName)) {
		errorMessage = "This step is not applicable for specific Syndication Partner";
		LOGGER.info("STEP " + stepNumber + " - ACTUAL: " + errorMessage);
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    } else {
		errorMessage = "This step is not applicable for DSL device";
		LOGGER.info("STEP " + stepNumber + " - ACTUAL: " + errorMessage);
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }

	    /**
	     * STEP 10: Verify if the XDNS feature is Disabled after factory reset using webpa param
	     * Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS
	     * <li>EXPECTED: XDNS Feature must be Disabled
	     */
	    // Test step number
	    stepNumber = "s10";
	    // boolean variable to store the status
	    status = false;
	    // Error message
	    errorMessage = "XDNS feature is enabled after factory resetting the device, which is not expected.";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : DESCRIPTION : Verify if the XDNS feature is Disabled after factory reset using webpa param Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : ACTION : Get XDNS feature status using webpa param Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS");
	    LOGGER.info("STEP :  " + stepNumber + " : EXPECTED: XDNS Feature must be disabled");
	    LOGGER.info("**********************************************************************************");
	    long startTime = System.currentTimeMillis();
	    do {
		response = tapEnv.executeWebPaCommand(device,
			BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_XDNS_FEATURE_STATUS);
		status = CommonMethods.isNotNull(response)
			&& CommonUtils.patternSearchFromTargetString(response, BroadBandTestConstants.FALSE);
	    } while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.TEN_MINUTE_IN_MILLIS
		    && DeviceModeHandler.isDSLDevice(device) && !status
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    if (status) {
		LOGGER.info("STEP 10: ACTUAL : XDNS Feature is disabled as expected.");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	} catch (TestException exception) {
	    LOGGER.error("TC-RDKB-XDNS-1000 : Execution error occured due to exception --> " + exception.getMessage());
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, exception.getMessage(), true);
	} finally {
	    try {
		BroadBandPostConditionUtils.executePostConditionForXdns(device, tapEnv, true);
		if (backupMap != null) {
		    BroadBandPostConditionUtils.executePostCondtDeleteTemporaryFilesInGateway(device, tapEnv,
			    BroadBandTestConstants.CONSTANT_3, backupMap);
		}
		LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	    } catch (TestException exception) {
		LOGGER.error(
			"TC-RDKB-XDNS-1000 : Execution error occurred while executing post conditions due to exception --> "
				+ exception.getMessage());
	    }
	}

	LOGGER.info("ENDING TEST CASE: TC-RDKB-XDNS-1000");

    }

    /**
     * Power cycle should persist the MFPConfig values
     * <ol>
     * <li>Retrive the default values of MFPConfig</li>
     * <li>Set the values of MFPConfig to non default value \"Optional\"</li>
     * <li>Reboot the device using WebPa params</li>
     * <li>Retrive the values of MFPConfig as \"Optional\"</li>
     * </ol>
     *
     * @param device
     *            {@link Instance of Dut}
     * 
     * @author kiruthiga.sakthivel
     * @refactor Athira
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestCategory.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-MFPCONFIG-1002")
    public void testToVerifyMFPConfigValuesPersistOverReboot(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-MFPCONFIG-102";
	String stepNum = "S1";
	String errorMessage = "";

	boolean status = false;
	BroadBandResultObject resultObj = null;
	List<BroadBandMFPConfigEnum> wifiAccessPoints = null;
	int postConStepNumber = 1;
	boolean isBussinessClassDevice = false;
	isBussinessClassDevice = DeviceModeHandler.isBusinessClassDevice(device);
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-MFPCONFIG-1002");
	LOGGER.info("TEST DESCRIPTION:  Power cycle should persist the MFPConfig values");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Retrive the default values of MFPConfig");
	LOGGER.info("2. Set the values of MFPConfig to non default value \"Optional\"");
	LOGGER.info("3. Reboot the device using WebPa params");
	LOGGER.info("4. Retrive the values of MFPConfig as \"Optional\"");
	LOGGER.info("#######################################################################################");

	try {

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");

	    /**
	     * PRE-CONDITION 1 : PERFORM FACTORY RESET ON THE DEVICE
	     */
	    BroadBandPreConditionUtils.executePreConditionToFactoryResetDevice(device, tapEnv,
		    BroadBandTestConstants.CONSTANT_1);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "S1";
	    errorMessage = "Failed to get the default values of MFPConfig";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Retrive the default values of MFPConfig");
	    LOGGER.info(

		    "STEP 1: ACTION : Execute webpa get command using parameter Device.WiFi.AccessPoint.{i}.Security.MFPConfig");
	    LOGGER.info("STEP 1: EXPECTED : By default the webpa response for MFPConfig should be Disabled,");
	    LOGGER.info("**********************************************************************************");

	    wifiAccessPoints = BroadBandWiFiUtils.getListofWifiAccessPoints();
	    resultObj = BroadBandWiFiUtils.verifyMFPConfigValues(tapEnv, device, wifiAccessPoints,
		    BroadBandTestConstants.DEFAULT_VALUE_MFPCONFIG_ACCESSPOINT, isBussinessClassDevice, true);
	    status = resultObj.isStatus();
	    errorMessage = resultObj.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Successfully verified the default values of MPFConfig of all VAPs");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "S2";
	    errorMessage = "Failed to perform set operation on MFPConfig";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Set the values of MFPConfig to non default value \"Optional\"");
	    LOGGER.info(
		    "STEP 2: ACTION : Execute webpa set command:Parameter: Parameter:Device.WiFi.AccessPoint.{i}.Security.MFPConfigData type: stringvalue: Optional");
	    LOGGER.info(
		    "STEP 2: EXPECTED : Webpa set operation should be success and value should be updated as Optional");

	    LOGGER.info("**********************************************************************************");

	    resultObj = BroadBandWiFiUtils.setMFPConfigValuesByWebPa(tapEnv, device, wifiAccessPoints,
		    BroadBandTestConstants.OPTIONAL_VALUE_MFPCONFIG_ACCESSPOINT, false);
	    status = resultObj.isStatus();
	    errorMessage = resultObj.getErrorMessage();
	    if (status) {
		status = false;
		resultObj = BroadBandWiFiUtils.verifyMFPConfigValues(tapEnv, device, wifiAccessPoints,
			BroadBandTestConstants.OPTIONAL_VALUE_MFPCONFIG_ACCESSPOINT, isBussinessClassDevice, false);
		status = resultObj.isStatus();
		errorMessage = resultObj.getErrorMessage();
	    }

	    if (status) {
		LOGGER.info(
			"STEP 2: ACTUAL : Successfully set and Verified the values of MPFConfig of all VAPs as Optional");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "S3";
	    errorMessage = "Not able to access box after initiating reboot";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Reboot the device using WebPa params");
	    LOGGER.info(
		    "STEP 3: ACTION : Reboot the device using WebPa params \"Device.X_CISCO_COM_DeviceControl.RebootDevice\" and verify reboot status");

	    LOGGER.info("STEP 3: EXPECTED : Box should reboot and should be accessable after reboot ");
	    LOGGER.info("**********************************************************************************");
	    status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);

	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : Successfully Rebooted the device");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "S4";
	    errorMessage = "Failed to get the values of MFPConfig as \"Optional\"";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : Retrive the values of MFPConfig as \"Optional\"");
	    LOGGER.info(
		    "STEP 4: ACTION : Execute webpa get command:Parameter:Device.WiFi.AccessPoint.{i}.Security.MFPConfig");
	    LOGGER.info("STEP 4: EXPECTED : Webpa response for MFPConfig should be Optional,");
	    LOGGER.info("**********************************************************************************");

	    wifiAccessPoints = BroadBandWiFiUtils.getListofWifiAccessPoints();
	    resultObj = BroadBandWiFiUtils.verifyMFPConfigValues(tapEnv, device, wifiAccessPoints,
		    BroadBandTestConstants.OPTIONAL_VALUE_MFPCONFIG_ACCESSPOINT, isBussinessClassDevice, false);
	    status = resultObj.isStatus();
	    errorMessage = resultObj.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Successfully verified the values of MPFConfig of all VAPs as Optional");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");
	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	} finally {

	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    BroadBandPostConditionUtils.postConditionTosetDefaultMFPConfigValues(device, tapEnv, postConStepNumber);
	    postConStepNumber++;
	    BroadBandPostConditionUtils.executePostConditionToReActivateDevice(device, tapEnv, false,
		    postConStepNumber);

	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");

	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-MFPCONFIG-1002");
    }

    /**
     * MFPConfig is configurable by Wi-Fi HAL
     * <ol>
     * <li>Retrive the default values of MFPConfig</li>
     * <li>Set the values of MFPConfig to \"Optional\" and verify the same via Wi-Fi HAL</li>
     * <li>Set the values of MFPConfig to \"Required\" and verify the same via Wi-Fi HAL</li>
     * <li>Set the values of MFPConfig to improper value \"Test\" and verify</li>
     * </ol>
     * 
     * @param device
     *            {@link Instanceof Dut}
     * 
     * @author kiruthiga.sakthivel
     * @refactor Athira
     * 
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
    @TestDetails(testUID = "TC-RDKB-MFPCONFIG-1003")
    public void testToVerifyMFPConfigValuesByWiFiHAL(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-MFPCONFIG-103";
	String stepNum = "S1";
	String errorMessage = "";
	boolean status = false;
	BroadBandResultObject resultObj = null;
	List<BroadBandMFPConfigEnum> wifiAccessPoints = null;
	String response = null;
	int postConStepNumber = 1;
	boolean isBusinessClassDevice = false;
	isBusinessClassDevice = DeviceModeHandler.isBusinessClassDevice(device);
	boolean isAtom = false;
	// Checking atom sync to verify WEBPA logs in atom console for Atom based devices
	isAtom = CommonMethods.isAtomSyncAvailable(device, tapEnv);
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-MFPCONFIG-1003");
	LOGGER.info("TEST DESCRIPTION:  MFPConfig is configurable by Wi-Fi HAL");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Retrive the default values of MFPConfig");
	LOGGER.info("2. Set the values of MFPConfig to  \"Optional\" and verify the same via Wi-Fi HAL");
	LOGGER.info("3. Set the values of MFPConfig to  \"Required\" and verify the same via Wi-Fi HAL");
	LOGGER.info("4. Set the values of MFPConfig to  improper value \"Test\" and verify ");

	LOGGER.info("#######################################################################################");

	try {

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    /**
	     * PRE-CONDITION 1 : PERFORM FACTORY RESET ON THE DEVICE
	     */
	    BroadBandPreConditionUtils.executePreConditionToFactoryResetDevice(device, tapEnv,
		    BroadBandTestConstants.CONSTANT_1);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "S1";
	    errorMessage = "Failed to get the default values of MFPConfig";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Retrive the default values of MFPConfig");
	    LOGGER.info(
		    "STEP 1: ACTION : Execute webpa get command:Parameter:Device.WiFi.AccessPoint.{i}.Security.MFPConfig");
	    LOGGER.info("STEP 1: EXPECTED : By default the webpa response for MFPConfig should be Disabled,");
	    LOGGER.info("**********************************************************************************");

	    wifiAccessPoints = BroadBandWiFiUtils.getListofWifiAccessPoints();
	    resultObj = BroadBandWiFiUtils.verifyMFPConfigValues(tapEnv, device, wifiAccessPoints,
		    BroadBandTestConstants.DEFAULT_VALUE_MFPCONFIG_ACCESSPOINT, isBusinessClassDevice, true);
	    status = resultObj.isStatus();
	    errorMessage = resultObj.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Successfully verified the default values of MPFConfig of all VAPs");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "S2";
	    errorMessage = "Failed to perform set operation on MFPConfig";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Set the values of MFPConfig to  \"Optional\" and verify the same via Wi-Fi HAL");
	    LOGGER.info(
		    "STEP 2: ACTION : Execute webpa set command:Parameter: Parameter:Device.WiFi.AccessPoint.{i}.Security.MFPConfigData type: stringvalue: Optional Execute Wi-Fi Hal API to get wifi_api wifi_getApSecurityMFPConfig {i}");
	    LOGGER.info(
		    "STEP 2: EXPECTED : Webpa set operation should be success and value should be updated as Optional");
	    LOGGER.info("**********************************************************************************");
	    wifiAccessPoints.clear();
	    wifiAccessPoints.add(BroadBandMFPConfigEnum.MFP_CONFIG_WIFI_AP_1);
	    wifiAccessPoints.add(BroadBandMFPConfigEnum.MFP_CONFIG_WIFI_AP_8);
	    LOGGER.info("*wifiAccessPoints.size *" + wifiAccessPoints.size());
	    resultObj = BroadBandWiFiUtils.setMFPConfigValuesByWebPa(tapEnv, device, wifiAccessPoints,
		    BroadBandTestConstants.OPTIONAL_VALUE_MFPCONFIG_ACCESSPOINT, false);
	    status = resultObj.isStatus();
	    errorMessage = resultObj.getErrorMessage();
	    if (status) {
		status = false;
		errorMessage = "Failed to get the MFPConfig value by WiFi HAL API";
		for (BroadBandMFPConfigEnum wifiAccessPoint : wifiAccessPoints) {
		    if (isAtom) {
			LOGGER.info("*isAtom *" + isAtom);
			response = tapEnv.executeCommandOnAtom(device,
				BroadBandCommandConstants.CMD_TO_GET_WIFI_AP_SECURITY_MFGCONFIG.replace(
					BroadBandTestConstants.STRING_REPLACE,
					String.valueOf(wifiAccessPoint.getWifiAPIIndex())));
		    } else {
			LOGGER.info("inside else isAtom *" + isAtom);
			response = tapEnv.executeCommandUsingSsh(device,
				BroadBandCommandConstants.CMD_TO_GET_WIFI_AP_SECURITY_MFGCONFIG.replace(
					BroadBandTestConstants.STRING_REPLACE,
					String.valueOf(wifiAccessPoint.getWifiAPIIndex())));
		    }
		    status = response.contains(BroadBandTestConstants.OPTIONAL_VALUE_MFPCONFIG_ACCESSPOINT);
		    LOGGER.info("* *" + status);
		    if (!status) {
			break;
		    }
		}
	    }
	    if (status) {
		LOGGER.info(
			"STEP 2: ACTUAL : Successfully set and Verified the values of MPFConfig oby WiFi HAL as Optional");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "S3";
	    errorMessage = "Failed to perform set operation on MFPConfig";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 3: DESCRIPTION : Set the values of MFPConfig to  \"Required\" and verify the same via Wi-Fi HAL");
	    LOGGER.info(
		    "STEP 3: ACTION : Execute webpa set command:Parameter: Parameter:Device.WiFi.AccessPoint.{i}.Security.MFPConfigData type: stringvalue: Required Execute Wi-Fi Hal API to get wifi_api wifi_getApSecurityMFPConfig {i}");
	    LOGGER.info(
		    "STEP 3: EXPECTED : Webpa set operation should be success and value should be updated as Required");
	    LOGGER.info("**********************************************************************************");
	    wifiAccessPoints.clear();
	    wifiAccessPoints.add(BroadBandMFPConfigEnum.MFP_CONFIG_WIFI_AP_2);
	    wifiAccessPoints.add(BroadBandMFPConfigEnum.MFP_CONFIG_WIFI_AP_9);
	    LOGGER.info("*wifiAccessPoints.size *" + wifiAccessPoints.size());
	    resultObj = BroadBandWiFiUtils.setMFPConfigValuesByWebPa(tapEnv, device, wifiAccessPoints,
		    BroadBandTestConstants.REQUIRED_VALUE_MFPCONFIG_ACCESSPOINT, false);
	    status = resultObj.isStatus();
	    errorMessage = resultObj.getErrorMessage();
	    if (status) {
		status = false;
		errorMessage = "Failed to get the MFPConfig value by WiFi HAL API";
		for (BroadBandMFPConfigEnum wifiAccessPoint : wifiAccessPoints) {
		    if (isAtom) {
			LOGGER.info("*isAtom *" + isAtom);
			response = tapEnv.executeCommandOnAtom(device,
				BroadBandCommandConstants.CMD_TO_GET_WIFI_AP_SECURITY_MFGCONFIG.replace(
					BroadBandTestConstants.STRING_REPLACE,
					String.valueOf(wifiAccessPoint.getWifiAPIIndex())));
		    } else {
			LOGGER.info("inside else isAtom *" + isAtom);
			response = tapEnv.executeCommandUsingSsh(device,
				BroadBandCommandConstants.CMD_TO_GET_WIFI_AP_SECURITY_MFGCONFIG.replace(
					BroadBandTestConstants.STRING_REPLACE,
					String.valueOf(wifiAccessPoint.getWifiAPIIndex())));
		    }

		    status = response.contains(BroadBandTestConstants.REQUIRED_VALUE_MFPCONFIG_ACCESSPOINT);
		    LOGGER.info("* *" + status);
		    if (!status) {
			break;
		    }
		}
	    }
	    if (status) {
		LOGGER.info(
			"STEP 3: ACTUAL : Successfully set and Verified the values of MPFConfig by WiFi HAL as Required");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "S4";
	    errorMessage = "Able to do set operation on MFPConfig with improper value";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : Set the values of MFPConfig to  improper value \"Test\" and verify ");
	    LOGGER.info(
		    "STEP 4: ACTION : Execute webpa set command:Parameter: Parameter:Device.WiFi.AccessPoint.{i}.Security.MFPConfigData type: stringvalue: Test");
	    LOGGER.info("STEP 4: EXPECTED : Webpa set operation should be failure ");
	    LOGGER.info("**********************************************************************************");

	    wifiAccessPoints = BroadBandWiFiUtils.getListofWifiAccessPoints();
	    resultObj = BroadBandWiFiUtils.setMFPConfigValuesByWebPa(tapEnv, device, wifiAccessPoints,
		    BroadBandTestConstants.STRING_DEVICE_NAME, false);
	    status = !resultObj.isStatus();
	    if (status) {
		LOGGER.info(
			"STEP 4: ACTUAL : Successfully Verified that values of MPFConfig cannot be set to improper value");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	} finally {

	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    BroadBandPostConditionUtils.postConditionTosetDefaultMFPConfigValues(device, tapEnv, postConStepNumber);
	    postConStepNumber++;
	    BroadBandPostConditionUtils.executePostConditionToReActivateDevice(device, tapEnv, false,
		    postConStepNumber);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-MFPCONFIG-1003");
    }

    /**
     * Verify force disable wifi radio functionality in bridge mode
     * <ol>
     * <li>Get default values of 2.4Ghz and 5Ghz radio status</li>
     * <li>set values of 2.4Ghz to true and 5Ghz to false</li>
     * <li>Set value of Force Disable WiFi parameter to true</li>
     * <li>Verify log message for Force Disable WiFi set to true</li>
     * <li>Verify 2.4Ghz radio has been disabled</li>
     * <li>Verify 5Ghz radio has been disabled</li>
     * <li>Verify radio enable write not allowed using webpa and log message</li>
     * <li>Verify AP enable write not allowed using webpa and log message</li>
     * <li>Verify SSID write not allowed using webpa and log message</li>
     * <li>Verify passphrase write not allowed using webpa and log message</li>
     * <li>Verify radio enable write not allowed using dmcli and log message</li>
     * <li>Verify AP enable write not allowed using dmcli and log message</li>
     * <li>Verify SSID write not allowed using dmcli and log message</li>
     * <li>Verify passphrase write not allowed using dmcli and log message</li>
     * <li>Verify radio enable write not allowed using snmp and log message</li>
     * <li>Verify SSID write not allowed using snmp and log message</li>
     * <li>Verify passphrase write not allowed using snmp and log message</li>
     * <li>Set value of Force Disable WiFi parameter to false</li>
     * <li>Verify log message for Force Disable WiFi set to false</li>
     * <li>Verify 2.4Ghz radio has been reset to original value</li>
     * <li>Verify 5Ghz radio has been reset to original value</li>
     * </ol>
     * 
     * @author Ashwin Sankarasubramanian,Betel Costrow
     * @refactor Athira
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-FORCE_DISABLE_WIFI-1002")
    public void testVerifyForceRadioDisableFeatureBridgeMode(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-FORCE_DISABLE_WIFI-102";
	String stepNum = "s1";
	String errorMessage = "";
	String radioStatus24 = null;
	String radioStatus5 = null;
	boolean radioEnable24 = false;
	boolean radioEnable5 = false;
	boolean status = false;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-FORCE_DISABLE_WIFI-1002");
	LOGGER.info("TEST DESCRIPTION: Verify force disable wifi radio functionality in bridge mode");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Get default values of 2.4Ghz and 5Ghz radio status");
	LOGGER.info("2. set values of 2.4Ghz to true and 5Ghz to false");
	LOGGER.info("3. Set value of Force Disable WiFi parameter to true");
	LOGGER.info("4. Verify log message for Force Disable WiFi set to true");
	LOGGER.info("5. Verify 2.4Ghz radio has been disabled");
	LOGGER.info("6. Verify 5Ghz radio has been disabled");
	LOGGER.info("7. Verify radio enable write not allowed using webpa and log message");
	LOGGER.info("8. Verify AP enable write not allowed using webpa and log message");
	LOGGER.info("9. Verify SSID write not allowed using webpa and log message");
	LOGGER.info("10. Verify passphrase write not allowed using webpa and log message");
	LOGGER.info("11. Verify radio enable write not allowed using dmcli and log message");
	LOGGER.info("12. Verify AP enable write not allowed using dmcli and log message");
	LOGGER.info("13. Verify SSID write not allowed using dmcli and log message");
	LOGGER.info("14. Verify passphrase write not allowed using dmcli and log message");
	LOGGER.info("15. Verify radio enable write not allowed using snmp and log message");
	LOGGER.info("16. Verify SSID write not allowed using snmp and log message");
	LOGGER.info("17. Verify passphrase write not allowed using snmp and log message");
	LOGGER.info("18. Set value of Force Disable WiFi parameter to false");
	LOGGER.info("19. Verify log message for Force Disable WiFi set to false");
	LOGGER.info("20. Verify 2.4Ghz radio has been reset to original value");
	LOGGER.info("21. Verify 5Ghz radio has been reset to original value");

	LOGGER.info("#######################################################################################");

	try {

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    LOGGER.info("PRE-CONDITION : DESCRIPTION : Set the device in bridge mode");
	    LOGGER.info(
		    "PRE-CONDITION : ACTION : Execute webpa or dmcli command to set value of Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode to bridge-static");
	    LOGGER.info("PRE-CONDITION : EXPECTED : Successfully set device in bridge mode");

	    errorMessage = "Failed to set device in bridge mode";
	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_BRIDGE_MODE_ENABLE, BroadBandTestConstants.CONSTANT_0,
		    BroadBandTestConstants.LAN_MANAGEMENT_MODE_BRIDGE_STATIC);

	    if (status) {
		LOGGER.info("PRE-CONDITION : ACTUAL : Pre condition executed successfully");
	    } else {
		LOGGER.error("PRE-CONDITION : ACTUAL : Pre condition failed");
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    }

	    LOGGER.info("**********************************************************************************");

	    errorMessage = "Failed to get value of 2.4G radio enable parameter";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Get default values of 2.4Ghz and 5Ghz radio status");
	    LOGGER.info(
		    "STEP 1: ACTION : Execute webpa or dmcli command to get values of Device.WiFi.Radio.1.Enable and Device.WiFi.Radio.2.Enable");
	    LOGGER.info("STEP 1: EXPECTED : Obtained values of 2.4Ghz and 5Ghz radio status");
	    LOGGER.info("**********************************************************************************");

	    radioStatus24 = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_RADIO_ENABLE);
	    if (CommonMethods.isNotNull(radioStatus24)) {
		errorMessage = "Failed to get value of 5G radio enable parameter";
		radioStatus5 = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_RADIO_ENABLE);
		status = CommonMethods.isNotNull(radioStatus5);
	    }

	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Obtained values of 2.4Ghz and 5Ghz radio status");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s2";
	    errorMessage = "Failed to enable 2.4Ghz radio parameter";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : set values of 2.4Ghz to true and 5Ghz to false");
	    LOGGER.info(
		    "STEP 2: ACTION : Execute webpa or dmcli command to set values of Device.WiFi.Radio.1.Enable to true and Device.WiFi.Radio.2.Enable false");
	    LOGGER.info("STEP 2: EXPECTED : Set values of 2.4Ghz and 5Ghz radio status parameter");
	    LOGGER.info("**********************************************************************************");

	    tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    if (radioStatus24.trim().equalsIgnoreCase(BroadBandTestConstants.TRUE)) {
		LOGGER.info("2.4Ghz radio has been enabled");
		radioEnable24 = true;
	    } else {
		radioEnable24 = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_RADIO_ENABLE, BroadBandTestConstants.CONSTANT_3,
			BroadBandTestConstants.TRUE);
	    }
	    errorMessage = "Failed to disable 5Ghz radio parameter";
	    if (radioStatus5.trim().equalsIgnoreCase(BroadBandTestConstants.TRUE)) {
		radioEnable5 = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_RADIO_ENABLE, BroadBandTestConstants.CONSTANT_3,
			BroadBandTestConstants.FALSE);

	    } else {
		LOGGER.info("5Ghz radio has been disabled ");
		radioEnable5 = true;
	    }
	    status = radioEnable24 && radioEnable5;

	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Successfully set 2.4Ghz to true and 5Ghz to false.");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    BroadbandRadioStatusWifiTest.executeForceWiFiDisableSetToTrueSteps(device, testCaseId,
		    BroadBandTestConstants.CONSTANT_3);

	    BroadbandRadioStatusWifiTest.executeWiFiConfigBlockedSteps(device, testCaseId,
		    BroadBandTestConstants.CONSTANT_7);

	    BroadbandRadioStatusWifiTest.executeForceWiFiDisableSetToFalseSteps(device, testCaseId,
		    BroadBandTestConstants.CONSTANT_18, BroadBandTestConstants.TRUE, BroadBandTestConstants.FALSE);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	} finally {

	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    LOGGER.info("POST-CONDITION : DESCRIPTION : Set the device in router mode");
	    LOGGER.info(
		    "POST-CONDITION : ACTION : Execute webpa or dmcli command to set value of Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode to router and"
			    + " dmcli command to set values of Device.WiFi.Radio.1.Enable and Device.WiFi.Radio.2.Enable from step 1");
	    LOGGER.info(
		    "POST-CONDITION : EXPECTED : Successfully set device in router mode and default values to 2.4Ghz & 5Ghz radio.");

	    if (BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_RADIO_ENABLE, BroadBandTestConstants.CONSTANT_3,
		    radioStatus24)) {
		status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_RADIO_ENABLE, BroadBandTestConstants.CONSTANT_3,
			radioStatus5);
	    }
	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_BRIDGE_MODE_ENABLE, BroadBandTestConstants.CONSTANT_0,
		    BroadBandTestConstants.LAN_MANAGEMENT_MODE_ROUTER);

	    if (status) {
		LOGGER.info("POST-CONDITION : ACTUAL : Post condition executed successfully");
	    } else {
		LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed");
	    }
	    LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-FORCE_DISABLE_WIFI-1002");
    }

    /**
     * Verify WiFi GAS TR-181 parameters and their default values
     * <ol>
     * <li>Verify Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.Queries is not writable</li>
     * <li>Verify Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.QueryRate is not writable</li>
     * <li>Verify Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.Responses is not writable</li>
     * <li>Verify Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.ResponseRate is not writable</li>
     * <li>Verify Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.NoRequestOutstanding is not writable</li>
     * <li>Verify Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.ResponsesDiscarded is not writable</li>
     * <li>Verify Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.FailedResponses is not writable</li>
     * <li>Verify GAS Configuration parameter is in json format</li>
     * <li>Update GAS Configuration parameter with new values</li>
     * <li>Reboot the device</li>
     * <li>Verify GAS configuration parameter value persists after reboot</li>
     * <li>Verify WiFi interworking is enabled for 2.4Ghz</li>
     * <li>Enable ANQP and Passpoint debugging for WiFi (on atom for Atomsync Devices)</li>
     * <li>Verify GAS request simulated by checking wifi debug logs</li>
     * <li>Verify GAS stats queries parameter is updated</li>
     * <li>Verify GAS stats responses parameter is updated</li>
     * <li>Perform factory reset using webpa</li>
     * <li>Verify default value of GAS configurations parameter</li>
     * <li>Verify default value of GAS Stats Queries parameter</li>
     * <li>Verify default value of GAS Stats QueryRate parameter</li>
     * <li>Verify default value of GAS Stats Responses parameter</li>
     * <li>Verify default value of GAS Stats ResponseRate parameter</li>
     * <li>Verify default value of GAS Stats NoRequestOutstanding parameter</li>
     * <li>Verify default value of GAS Stats ResponsesDiscarded parameter</li>
     * <li>Verify default value of GAS Stats FailedResponses parameter</li>
     * </ol>
     * 
     * @author Ashwin sankara
     * @refactor Govardhan
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WIFI_GAS-1001")
    public void testVerifyWifiGasConfigurationParameters(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-WIFI_GAS-101";
	String stepNum = "s1";
	String errorMessage = null;
	String response = null;
	boolean status = false;
	// Variable Declation Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI_GAS-1001");
	LOGGER.info("TEST DESCRIPTION: Verify WiFi GAS TR-181 parameters and their default values");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.Queries is not writable");
	LOGGER.info("2. Verify Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.QueryRate is not writable");
	LOGGER.info("3. Verify Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.Responses is not writable");
	LOGGER.info("4. Verify Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.ResponseRate is not writable");
	LOGGER.info("5. Verify Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.NoRequestOutstanding is not writable");
	LOGGER.info("6. Verify Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.ResponsesDiscarded is not writable");
	LOGGER.info("7. Verify Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.FailedResponses is not writable");
	LOGGER.info("8. Verify GAS Configuration parameter is in json format");
	LOGGER.info("9. Update GAS Configuration parameter with new values");
	LOGGER.info("10. Reboot the device");
	LOGGER.info("11. Verify GAS configuration parameter value persists after reboot");
	LOGGER.info("12. Verify WiFi interworking is enabled for 2.4Ghz");
	LOGGER.info("13. Enable ANQP and Passpoint debugging for WiFi (on atom for Atomsync Devices)");
	LOGGER.info("14. Verify GAS request simulated by checking wifi debug logs");
	LOGGER.info("15. Verify GAS stats queries parameter is updated");
	LOGGER.info("16. Verify GAS stats responses parameter is updated");
	LOGGER.info("17. Perform factory reset using webpa");
	LOGGER.info("18. Verify default value of GAS configurations parameter");
	LOGGER.info("19. Verify default value of GAS Stats Queries parameter");
	LOGGER.info("20. Verify default value of GAS Stats QueryRate parameter");
	LOGGER.info("21. Verify default value of GAS Stats Responses parameter");
	LOGGER.info("22. Verify default value of GAS Stats ResponseRate parameter");
	LOGGER.info("23. Verify default value of GAS Stats NoRequestOutstanding parameter");
	LOGGER.info("24. Verify default value of GAS Stats ResponsesDiscarded parameter");
	LOGGER.info("25. Verify default value of GAS Stats FailedResponses parameter");

	LOGGER.info("#######################################################################################");

	try {

	    stepNum = "s1";
	    errorMessage = "Able to set value for Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.Queries";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.Queries is not writable");
	    LOGGER.info(
		    "STEP 1: ACTION : Execute dmcli command to set Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.Queries to value uint 1");
	    LOGGER.info("STEP 1: EXPECTED : Dmcli set failed with not writable error");
	    LOGGER.info("**********************************************************************************");

	    status = !DmcliUtils.setParameterValueUsingDmcliCommand(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFI_GAS_STATS_QUERIES,
		    BroadBandTestConstants.DMCLI_SUFFIX_TO_SET_UINT_PARAMETER, BroadBandTestConstants.STRING_VALUE_ONE);

	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Dmcli set failed with not writable error");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s2";
	    errorMessage = "Able to set value for Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.QueryRate";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Verify Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.QueryRate is not writable");
	    LOGGER.info(
		    "STEP 2: ACTION : Execute dmcli command to set Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.QueryRate to value uint 1");
	    LOGGER.info("STEP 2: EXPECTED : Dmcli set failed with not writable error");
	    LOGGER.info("**********************************************************************************");

	    status = !DmcliUtils.setParameterValueUsingDmcliCommand(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFI_GAS_STATS_QUERY_RATE,
		    BroadBandTestConstants.DMCLI_SUFFIX_TO_SET_UINT_PARAMETER, BroadBandTestConstants.STRING_VALUE_ONE);

	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Dmcli set failed with not writable error");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s3";
	    errorMessage = "Able to set value for Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.Responses";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 3: DESCRIPTION : Verify Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.Responses is not writable");
	    LOGGER.info(
		    "STEP 3: ACTION : Execute dmcli command to set Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.Responses to value uint 1");
	    LOGGER.info("STEP 3: EXPECTED : Dmcli set failed with not writable error");
	    LOGGER.info("**********************************************************************************");

	    status = !DmcliUtils.setParameterValueUsingDmcliCommand(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFI_GAS_STATS_RESPONSES,
		    BroadBandTestConstants.DMCLI_SUFFIX_TO_SET_UINT_PARAMETER, BroadBandTestConstants.STRING_VALUE_ONE);

	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : Dmcli set failed with not writable error");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s4";
	    errorMessage = "Able to set value for Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.ResponseRate";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 4: DESCRIPTION : Verify Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.ResponseRate is not writable");
	    LOGGER.info(
		    "STEP 4: ACTION : Execute dmcli command to set Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.ResponseRate to value uint 1");
	    LOGGER.info("STEP 4: EXPECTED : Dmcli set failed with not writable error");
	    LOGGER.info("**********************************************************************************");

	    status = !DmcliUtils.setParameterValueUsingDmcliCommand(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFI_GAS_STATS_RESPONSE_RATE,
		    BroadBandTestConstants.DMCLI_SUFFIX_TO_SET_UINT_PARAMETER, BroadBandTestConstants.STRING_VALUE_ONE);

	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Dmcli set failed with not writable error");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s5";
	    errorMessage = "Able to set value for Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.NoRequestOutstanding";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 5: DESCRIPTION : Verify Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.NoRequestOutstanding is not writable");
	    LOGGER.info(
		    "STEP 5: ACTION : Execute dmcli command to set Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.NoRequestOutstanding to value uint 1");
	    LOGGER.info("STEP 5: EXPECTED : Dmcli set failed with not writable error");
	    LOGGER.info("**********************************************************************************");

	    status = !DmcliUtils.setParameterValueUsingDmcliCommand(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFI_GAS_STATS_REQUEST_OUTSTANDING,
		    BroadBandTestConstants.DMCLI_SUFFIX_TO_SET_UINT_PARAMETER, BroadBandTestConstants.STRING_VALUE_ONE);

	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : Dmcli set failed with not writable error");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s6";
	    errorMessage = "Able to set value for Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.ResponsesDiscarded";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 6: DESCRIPTION : Verify Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.ResponsesDiscarded is not writable");
	    LOGGER.info(
		    "STEP 6: ACTION : Execute dmcli command to set Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.ResponsesDiscarded to value uint 1");
	    LOGGER.info("STEP 6: EXPECTED : Dmcli set failed with not writable error");
	    LOGGER.info("**********************************************************************************");

	    status = !DmcliUtils.setParameterValueUsingDmcliCommand(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFI_GAS_STATS_RESPONSES_DISCARDED,
		    BroadBandTestConstants.DMCLI_SUFFIX_TO_SET_UINT_PARAMETER, BroadBandTestConstants.STRING_VALUE_ONE);

	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : Dmcli set failed with not writable error");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s7";
	    errorMessage = "Able to set value for Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.FailedResponses";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 7: DESCRIPTION : Verify Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.FailedResponses is not writable");
	    LOGGER.info(
		    "STEP 7: ACTION : Execute dmcli command to set Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.FailedResponses to value uint 1");
	    LOGGER.info("STEP 7: EXPECTED : Dmcli set failed with not writable error");
	    LOGGER.info("**********************************************************************************");

	    status = !DmcliUtils.setParameterValueUsingDmcliCommand(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFI_GAS_STATS_FAILED_RESPONSES,
		    BroadBandTestConstants.DMCLI_SUFFIX_TO_SET_UINT_PARAMETER, BroadBandTestConstants.STRING_VALUE_ONE);

	    if (status) {
		LOGGER.info("STEP 7: ACTUAL : Dmcli set failed with not writable error");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s8";
	    errorMessage = "Failed to get value of Device.WiFi.X_RDKCENTRAL-COM_GASConfiguration";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION : Verify GAS Configuration parameter is in json format");
	    LOGGER.info(
		    "STEP 8: ACTION : 1. Execute dmcli command to get value of Device.WiFi.X_RDKCENTRAL-COM_GASConfiguration2. Verify parameter value is in json format");
	    LOGGER.info("STEP 8: EXPECTED : Successfully verified GAS configuration parameter value is in json format");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFI_GAS_CONFIGURATION);
	    if (CommonMethods.isNotNull(response)) {
		errorMessage = "Value of GAS configuration parameter is not in json format";
		status = CommonMethods.isValidJsonString(response);
	    }

	    if (status) {
		LOGGER.info(
			"STEP 8: ACTUAL : Successfully verified GAS configuration parameter value is in json format");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s9";
	    errorMessage = "Failed to set value of Device.WiFi.X_RDKCENTRAL-COM_GASConfiguration";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 9: DESCRIPTION : Update GAS Configuration parameter with new values");
	    LOGGER.info(
		    "STEP 9: ACTION : Execute webpa command to set Device.WiFi.X_RDKCENTRAL-COM_GASConfiguration with value: "
			    + BroadBandTestConstants.STRING_GAS_CONFIG_JSON_VALUE);
	    LOGGER.info("STEP 9: EXPECTED : Successfully set GAS configuration parameter with new value");
	    LOGGER.info("**********************************************************************************");

	    JSONObject json = new JSONObject(BroadBandTestConstants.STRING_GAS_CONFIG_JSON_VALUE);
	    try {
		if (BroadBandWiFiUtils.setWebPaParams(device,
			BroadBandWebPaConstants.WEBPA_PARAM_WIFI_GAS_CONFIGURATION, json.toString(),
			BroadBandTestConstants.CONSTANT_0))

		{
		    errorMessage = "GAS config parameter value is not updated after WebPA set";
		    status = BroadBandWebPaUtils.verifyJsonParameterValue(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_WIFI_GAS_CONFIGURATION, json);
		}
	    } catch (Exception e) {
		LOGGER.error(e.getMessage());
	    }
	    try {
		if (!status) {

		    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_WIFI_GAS_CONFIGURATION,
			    BroadBandTestConstants.CONSTANT_0, json.toString());
		}
	    } catch (Exception e) {
		LOGGER.error(e.getMessage());
	    }

	    if (status) {
		LOGGER.info("STEP 9: ACTUAL : Successfully set GAS configuration parameter with new value");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s10";
	    errorMessage = "Failed to reboot the device";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 10: DESCRIPTION : Reboot the device");
	    LOGGER.info("STEP 10: ACTION : Execute command:/sbin/reboot");
	    LOGGER.info("STEP 10: EXPECTED : Device rebooted successfully");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);

	    if (status) {
		LOGGER.info("STEP 10: ACTUAL : Device rebooted successfully");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s11";
	    errorMessage = "Failed to verify value of Device.WiFi.X_RDKCENTRAL-COM_GASConfiguration persists after reboot";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 11: DESCRIPTION : Verify GAS configuration parameter value persists after reboot");
	    LOGGER.info(
		    "STEP 11: ACTION : Execute dmcli command to get value of Device.WiFi.X_RDKCENTRAL-COM_GASConfiguration");
	    LOGGER.info("STEP 11: EXPECTED : Value of GAS configuration parameter persists after reboot");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.verifyJsonParameterValue(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFI_GAS_CONFIGURATION, json);

	    if (status) {
		LOGGER.info("STEP 11: ACTUAL : Value of GAS configuration parameter persists after reboot");
	    } else {
		LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		stepNum = "s12";
		errorMessage = "Failed to set value of Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.WiFi-Interworking.Enable";
		status = false;

		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 12: DESCRIPTION : Verify WiFi interworking is enabled for 2.4Ghz");
		LOGGER.info(
			"STEP 12: ACTION : Execute webpa commands to set Device.WiFi.AccessPoint.10001.X_RDKCENTRAL-COM_InterworkingServiceEnable and Device.WiFi.AccessPoint.10001.X_RDKCENTRAL-COM_InterworkingApplySettings to true");
		LOGGER.info("STEP 12: EXPECTED : Value of WiFi interworking for 2.4Ghz parameter is true");
		LOGGER.info("**********************************************************************************");

		status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_FOR_IE_ENABLE, BroadBandTestConstants.TRUE,
			BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.CONSTANT_0)
			&& BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_FOR_IE_SERVICE_ENABLE.replace(
					BroadBandTestConstants.TR181_NODE_REF,
					BroadBandWebPaConstants.WEBPA_INDEX_2_4_GHZ_PRIVATE_SSID),
				BroadBandTestConstants.TRUE, BroadBandTestConstants.CONSTANT_0,
				BroadBandTestConstants.CONSTANT_0);
		if (!status) {
		    if (BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_FOR_IE_ENABLE, BroadBandTestConstants.CONSTANT_3,
			    BroadBandTestConstants.TRUE)) {
			tapEnv.waitTill(BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
			errorMessage = "Failed to set value of Device.WiFi.AccessPoint.10001.X_RDKCENTRAL-COM_InterworkingServiceEnable";
			if (BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_FOR_IE_SERVICE_ENABLE.replace(
					BroadBandTestConstants.TR181_NODE_REF,
					BroadBandWebPaConstants.WEBPA_INDEX_2_4_GHZ_PRIVATE_SSID),
				BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE)) {
			    tapEnv.waitTill(BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
			    errorMessage = "Failed to set value of Device.WiFi.AccessPoint.10001.X_RDKCENTRAL-COM_InterworkingApplySettings";
			    status = BroadBandWiFiUtils.setWebPaParams(device,
				    BroadBandWebPaConstants.WEBPA_PARAM_FOR_IE_SETTINGS.replace(
					    BroadBandTestConstants.TR181_NODE_REF,
					    BroadBandWebPaConstants.WEBPA_INDEX_2_4_GHZ_PRIVATE_SSID),
				    BroadBandTestConstants.TRUE, BroadBandTestConstants.CONSTANT_3);
			}
		    }
		}

		if (status) {
		    LOGGER.info("STEP 12: ACTUAL : Value of WiFi interworking for 2.4Ghz parameter is true");
		} else {
		    LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

		LOGGER.info("**********************************************************************************");

		stepNum = "s13";
		errorMessage = "Unable to touch file /nvram/wifiAnqpDbg";
		status = false;

		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP 13: DESCRIPTION : Enable ANQP and Passpoint debugging for WiFi (on atom for Atomsync Devices)");
		LOGGER.info(
			"STEP 13: ACTION : Execute commands: touch /nvram/wifiAnqpDbgtouch /nvram/wifiPasspointDbg");
		LOGGER.info(
			"STEP 13: EXPECTED : Files /nvram/wifiAnqpDbg and /nvram/wifiPasspointDbg are created (on atom for Atomsync Devices)");
		LOGGER.info("**********************************************************************************");

		tapEnv.executeCommandOnAtom(device,
			BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_TOUCH,
				BroadBandCommandConstants.FILE_PATH_WIFI_ANQP_DEBUG,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
				BroadBandCommandConstants.FILE_PATH_WIFI_PASSPOINT_DEBUG));
		if (BroadBandCommonUtils.isFileExistsonAtom(device, tapEnv,
			BroadBandCommandConstants.FILE_PATH_WIFI_ANQP_DEBUG)) {
		    errorMessage = "Unable to touch file /nvram/wifiPasspointDbg";
		    status = BroadBandCommonUtils.isFileExistsonAtom(device, tapEnv,
			    BroadBandCommandConstants.FILE_PATH_WIFI_PASSPOINT_DEBUG);
		}

		if (status) {
		    LOGGER.info(
			    "STEP 13: ACTUAL : Files /nvram/wifiAnqpDbg and /nvram/wifiPasspointDbg are created (on atom for Atomsync Devices)");
		} else {
		    LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

		LOGGER.info("**********************************************************************************");

		stepNum = "s14";
		errorMessage = "Failed to find log message for gas response send";
		status = false;

		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 14: DESCRIPTION : Verify GAS request simulated by checking wifi debug logs");
		LOGGER.info(
			"STEP 14: ACTION : Execute command:wifi_api wifi_anqpStartTest ath0 aa:bb:cc:dd:ee:ffgrep \"Interworking is enabled will proceed to gas response send on AP: 1\" /tmp/wifiAnqp");
		LOGGER.info("STEP 14: EXPECTED : GAS request and response simulation verified with debug logs");
		LOGGER.info("**********************************************************************************");

		tapEnv.executeCommandOnAtom(device, BroadBandCommandConstants.CMD_SIMULATE_24GHZ_GAS_REQUEST);
		status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device,
			tapEnv, BroadBandTraceConstants.LOG_MESSAGE_GAS_REQUEST_AP,
			BroadBandCommandConstants.FILE_PATH_TMP_WIFI_ANQP, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
			BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS));

		if (status) {
		    LOGGER.info("STEP 14: ACTUAL : GAS request and response simulation verified with debug logs");
		} else {
		    LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		LOGGER.info("**********************************************************************************");

		stepNum = "s15";
		errorMessage = "Failed to get value of Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.Queries";
		status = false;

		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 15: DESCRIPTION : Verify GAS stats queries parameter is updated");
		LOGGER.info(
			"STEP 15: ACTION : Execute dmcli command to get Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.Queries");
		LOGGER.info("STEP 15: EXPECTED : GAS Stats Queries parameter has value greater than 0");
		LOGGER.info("**********************************************************************************");

		response = DmcliUtils.getParameterValueUsingDmcliCommand(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_WIFI_GAS_STATS_RESPONSES);
		if (CommonMethods.isNotNull(response)) {
		    errorMessage = "Value of Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.Queries is not greater than 0 after simulating GAS request";
		    status = Integer.parseInt(response.trim()) > BroadBandTestConstants.CONSTANT_0;
		}

		if (status) {
		    LOGGER.info("STEP 15: ACTUAL : GAS Stats Queries parameter has value greater than 0");
		} else {
		    LOGGER.error("STEP 15: ACTUAL : " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		LOGGER.info("**********************************************************************************");

		stepNum = "s16";
		errorMessage = "Failed to get value of Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.Responses";
		status = false;

		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 16: DESCRIPTION : Verify GAS stats responses parameter is updated");
		LOGGER.info(
			"STEP 16: ACTION : Execute dmcli command to get Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.Responses");
		LOGGER.info("STEP 16: EXPECTED : GAS Stats Responses parameter has value greater than 0");
		LOGGER.info("**********************************************************************************");

		response = DmcliUtils.getParameterValueUsingDmcliCommand(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_WIFI_GAS_STATS_RESPONSES);
		if (CommonMethods.isNotNull(response)) {
		    errorMessage = "Value of Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.Responses is not greater than 0 after simulating GAS request";
		    status = Integer.parseInt(response.trim()) > BroadBandTestConstants.CONSTANT_0;
		}

		if (status) {
		    LOGGER.info("STEP 16: ACTUAL : GAS Stats Responses parameter has value greater than 0");
		} else {
		    LOGGER.error("STEP 16: ACTUAL : " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		LOGGER.info("**********************************************************************************");
	    } else {
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 12 - 16 is not applicable for non AtomSync models");
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, "s12", ExecutionStatus.NOT_APPLICABLE,
			"Not applicable for non AtomSync models", false);
		tapEnv.updateExecutionForAllStatus(device, testCaseId, "s13", ExecutionStatus.NOT_APPLICABLE,
			"Not applicable for non AtomSync models", false);
		tapEnv.updateExecutionForAllStatus(device, testCaseId, "s14", ExecutionStatus.NOT_APPLICABLE,
			"Not applicable for non AtomSync models", false);
		tapEnv.updateExecutionForAllStatus(device, testCaseId, "s15", ExecutionStatus.NOT_APPLICABLE,
			"Not applicable for non AtomSync models", false);
		tapEnv.updateExecutionForAllStatus(device, testCaseId, "s16", ExecutionStatus.NOT_APPLICABLE,
			"Not applicable for non AtomSync models", false);
	    }

	    stepNum = "s17";
	    errorMessage = "Failed to perform factory reset using webpa";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 17: DESCRIPTION : Perform factory reset using webpa");
	    LOGGER.info(
		    "STEP 17: ACTION : Execute webpa command to set Device.X_CISCO_COM_DeviceControl.FactoryReset to Router,Wifi,VoIP,Dect,MoCA");
	    LOGGER.info("STEP 17: EXPECTED : Successfully performed factory reset");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandCommonUtils.performFactoryResetAndWaitForWebPaProcessToUp(tapEnv, device);

	    if (status) {
		LOGGER.info("STEP 17: ACTUAL : Successfully performed factory reset");
	    } else {
		LOGGER.error("STEP 17: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s18";
	    errorMessage = "Failed to verify Device.WiFi.X_RDKCENTRAL-COM_GASConfiguration value after factory reset";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 18: DESCRIPTION : Verify default value of GAS configurations parameter");
	    LOGGER.info(
		    "STEP 18: ACTION : Execute dmcli command to get value of Device.WiFi.X_RDKCENTRAL-COM_GASConfiguration");
	    LOGGER.info("STEP 18: EXPECTED : Default value of GAS configurations is present after factory reset");
	    LOGGER.info("**********************************************************************************");

	    json = new JSONObject(BroadBandTestConstants.STRING_GAS_CONFIG_DEFAULT_VALUE);
	    status = BroadBandWebPaUtils.verifyJsonParameterValue(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFI_GAS_CONFIGURATION, json);

	    if (status) {
		LOGGER.info("STEP 18: ACTUAL : Default value of GAS configurations is present after factory reset");
	    } else {
		LOGGER.error("STEP 18: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s19";
	    errorMessage = "Failed to get value of Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.Queries";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 19: DESCRIPTION : Verify default value of GAS Stats Queries parameter");
	    LOGGER.info(
		    "STEP 19: ACTION : Execute dmcli command to get value of Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.Queries");
	    LOGGER.info("STEP 19: EXPECTED : Default value of GAS Stat Queries is verified");
	    LOGGER.info("**********************************************************************************");

	    response = DmcliUtils.getParameterValueUsingDmcliCommand(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFI_GAS_STATS_QUERIES);
	    if (CommonMethods.isNotNull(response)) {
		errorMessage = "Failed to verify Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.Queries value after factory reset. Default value: 0, Actual value: "
			+ response;
		status = response.equalsIgnoreCase(BroadBandTestConstants.STRING_ZERO);
	    }

	    if (status) {
		LOGGER.info("STEP 19: ACTUAL : Default value of GAS Stat Queries is verified");
	    } else {
		LOGGER.error("STEP 19: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s20";
	    errorMessage = "Failed to get value of Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.QueryRate";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 20: DESCRIPTION : Verify default value of GAS Stats QueryRate parameter");
	    LOGGER.info(
		    "STEP 20: ACTION : Execute dmcli command to get value of Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.QueryRate");
	    LOGGER.info("STEP 20: EXPECTED : Default value of GAS Stat QueryRate is verified");
	    LOGGER.info("**********************************************************************************");

	    response = DmcliUtils.getParameterValueUsingDmcliCommand(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFI_GAS_STATS_QUERY_RATE);
	    if (CommonMethods.isNotNull(response)) {
		errorMessage = "Failed to verify Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.QueryRate value after factory reset. Default value: 0, Actual value: "
			+ response;
		status = response.equalsIgnoreCase(BroadBandTestConstants.STRING_ZERO);
	    }

	    if (status) {
		LOGGER.info("STEP 20: ACTUAL : Default value of GAS Stat QueryRate is verified");
	    } else {
		LOGGER.error("STEP 20: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s21";
	    errorMessage = "Failed to get value of Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.Responses";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 21: DESCRIPTION : Verify default value of GAS Stats Responses parameter");
	    LOGGER.info(
		    "STEP 21: ACTION : Execute dmcli command to get value of Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.Responses");
	    LOGGER.info("STEP 21: EXPECTED : Default value of GAS Stat Responses is verified");
	    LOGGER.info("**********************************************************************************");

	    response = DmcliUtils.getParameterValueUsingDmcliCommand(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFI_GAS_STATS_RESPONSES);
	    if (CommonMethods.isNotNull(response)) {
		errorMessage = "Failed to verify Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.Responses value after factory reset. Default value: 0, Actual value: "
			+ response;
		status = response.equalsIgnoreCase(BroadBandTestConstants.STRING_ZERO);
	    }

	    if (status) {
		LOGGER.info("STEP 21: ACTUAL : Default value of GAS Stat Responses is verified");
	    } else {
		LOGGER.error("STEP 21: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s22";
	    errorMessage = "Failed to get value of Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.ResponseRate";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 22: DESCRIPTION : Verify default value of GAS Stats ResponseRate parameter");
	    LOGGER.info(
		    "STEP 22: ACTION : Execute dmcli command to get value of Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.ResponseRate");
	    LOGGER.info("STEP 22: EXPECTED : Default value of GAS Stat ResponseRate is verified");
	    LOGGER.info("**********************************************************************************");

	    response = DmcliUtils.getParameterValueUsingDmcliCommand(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFI_GAS_STATS_RESPONSE_RATE);
	    if (CommonMethods.isNotNull(response)) {
		errorMessage = "Failed to verify Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.ResponseRate value after factory reset. Default value: 0, Actual value: "
			+ response;
		status = response.equalsIgnoreCase(BroadBandTestConstants.STRING_ZERO);
	    }

	    if (status) {
		LOGGER.info("STEP 22: ACTUAL : Default value of GAS Stat ResponseRate is verified");
	    } else {
		LOGGER.error("STEP 22: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s23";
	    errorMessage = "Failed to get value of Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.NoRequestOutstanding";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 23: DESCRIPTION : Verify default value of GAS Stats NoRequestOutstanding parameter");
	    LOGGER.info(
		    "STEP 23: ACTION : Execute dmcli command to get value of Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.NoRequestOutstanding");
	    LOGGER.info("STEP 23: EXPECTED : Default value of GAS Stat NoRequestOutstanding is verified");
	    LOGGER.info("**********************************************************************************");

	    response = DmcliUtils.getParameterValueUsingDmcliCommand(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFI_GAS_STATS_REQUEST_OUTSTANDING);
	    if (CommonMethods.isNotNull(response)) {
		errorMessage = "Failed to verify Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.NoRequestOutstanding value after factory reset. Default value: 0, Actual value: "
			+ response;
		status = response.equalsIgnoreCase(BroadBandTestConstants.STRING_ZERO);
	    }

	    if (status) {
		LOGGER.info("STEP 23: ACTUAL : Default value of GAS Stat NoRequestOutstanding is verified");
	    } else {
		LOGGER.error("STEP 23: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s24";
	    errorMessage = "Failed to get value of Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.ResponsesDiscarded";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 24: DESCRIPTION : Verify default value of GAS Stats ResponsesDiscarded parameter");
	    LOGGER.info(
		    "STEP 24: ACTION : Execute dmcli command to get value of Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.ResponsesDiscarded");
	    LOGGER.info("STEP 24: EXPECTED : Default value of GAS Stat ResponsesDiscarded is verified");
	    LOGGER.info("**********************************************************************************");

	    response = DmcliUtils.getParameterValueUsingDmcliCommand(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFI_GAS_STATS_RESPONSES_DISCARDED);
	    if (CommonMethods.isNotNull(response)) {
		errorMessage = "Failed to verify Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.ResponsesDiscarded value after factory reset. Default value: 0, Actual value: "
			+ response;
		status = response.equalsIgnoreCase(BroadBandTestConstants.STRING_ZERO);
	    }

	    if (status) {
		LOGGER.info("STEP 24: ACTUAL : Default value of GAS Stat ResponsesDiscarded is verified");
	    } else {
		LOGGER.error("STEP 24: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s25";
	    errorMessage = "Failed to get value of Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.FailedResponses";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 25: DESCRIPTION : Verify default value of GAS Stats FailedResponses parameter");
	    LOGGER.info(
		    "STEP 25: ACTION : Execute dmcli command to get value of Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.FailedResponses");
	    LOGGER.info("STEP 25: EXPECTED : Default value of GAS Stat FailedResponses is verified");
	    LOGGER.info("**********************************************************************************");

	    response = DmcliUtils.getParameterValueUsingDmcliCommand(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFI_GAS_STATS_FAILED_RESPONSES);
	    if (CommonMethods.isNotNull(response)) {
		errorMessage = "Failed to verify Device.WiFi.X_RDKCENTRAL-COM_GASStats.1.FailedResponses value after factory reset. Default value: 0, Actual value: "
			+ response;
		status = response.equalsIgnoreCase(BroadBandTestConstants.STRING_ZERO);
	    }

	    if (status) {
		LOGGER.info("STEP 25: ACTUAL : Default value of GAS Stat FailedResponses is verified");
	    } else {
		LOGGER.error("STEP 25: ACTUAL : " + errorMessage);
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
	    LOGGER.info("POST-CONDITION 1: DESCRIPTION :  Remove /nvram/wifiAnqpDbg file");
	    LOGGER.info("POST-CONDITION 1: ACTION : Execute command:rm -rf /nvram/wifiAnqpDbg");
	    LOGGER.info("POST-CONDITION 1: EXPECTED : Removed WiFi Anqp debug file successfully");

	    status = CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device,
		    BroadBandCommandConstants.FILE_PATH_WIFI_ANQP_DEBUG);

	    if (status) {
		LOGGER.info("POST-CONDITION 1: ACTUAL : Removed WiFi Anqp debug file successfully");
	    } else {
		LOGGER.error("POST-CONDITION 1: ACTUAL : Failed to remove WiFi Anqp debug file from nvram");
	    }

	    LOGGER.info("**********************************************************************************");

	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("POST-CONDITION 2: DESCRIPTION : Remove /nvram/wifiPasspointDbg file");
	    LOGGER.info("POST-CONDITION 2: ACTION : Execute command:rm -rf /nvram/wifiPasspointDbg");
	    LOGGER.info("POST-CONDITION 2: EXPECTED : Removed WiFi Passpoint debug file successfully");
	    LOGGER.info("**********************************************************************************");

	    status = CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device,
		    BroadBandCommandConstants.FILE_PATH_WIFI_PASSPOINT_DEBUG);

	    if (status) {
		LOGGER.info("POST-CONDITION 2: ACTUAL : Removed WiFi Passpoint debug file successfully");
	    } else {
		LOGGER.error("POST-CONDITION 2: ACTUAL : Failed to remove WiFi Passpoint debug file from nvram");
	    }

	    LOGGER.info("**********************************************************************************");

	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("POST-CONDITION 3: DESCRIPTION : Reactivate broadband gateway after factory reset");
	    LOGGER.info(
		    "POST-CONDITION 3: ACTION : Execute webpa commands to set ssid, passwords, configurewifi and captive portal enable as false");
	    LOGGER.info("POST-CONDITION 3: EXPECTED : Device wifi reactivated successfully");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWiFiUtils.reactivateDeviceUsingWebpaOrSnmp(tapEnv, device);

	    if (status) {
		LOGGER.info("POST-CONDITION 3: ACTUAL : Device wifi reactivated successfully");
	    } else {
		LOGGER.error("POST-CONDITION 3: ACTUAL : Failed to reactivate gateway after factory reset");
	    }

	    LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");

	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI_GAS-1001");
    }
    
    /**
     * 
     * 
     * Test Case : Verify XDNS in Process Monitor after reboot
     * 
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>Step 1 : Reboot the gateway device.</li>
     * <li>Step 2 : Verify upnp version details</li>
     * <li>Step 3 : Verify the CcspXdnsSsp process is up and running on the device</li>
     * <li>Step 4 : kill and verify the CcspXdnsSsp process</li>
     * <li>Step 5 : Verify log 'Resetting process CcspXdnsSsp' is available in /rdklogs/logs/SelfHeal.txt.0. For DSL
     * log should be in systemd_processRestart.log</li> *
     * <li>Step 6 : Set and verify the XDNS status</li>
     * <li>POST-CONDITION 1 : Verify the xdns status is disabled</li>
     * </ol>
     * 
     * @param device
     *            {@link Dut}
     *
     * @author Muthukumar
     * @refactor yamini.s
     *
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-CCSP-XDNS-5001")
    public void testToVerifyXDNSProcessAfterReboot(Dut device) {
	String testCaseId = "TC-RDKB-CCSP-XDNS-501";
	String response = null;
	boolean isSystemdPlatforms = false;
	isSystemdPlatforms = BroadbandPropertyFileHandler.isDeviceCheckForXDNS(device) || (DeviceModeHandler.isDSLDevice(device))
		|| DeviceModeHandler.isFibreDevice(device);
	int stepNumber = 1;
	stepNum = "S" + stepNumber;
	try {
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-CCSP-XDNS-5001");
	    LOGGER.info("TEST DESCRIPTION: Verify XDNS in Process Monitor after reboot");	    
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info("1. Reboot the gateway device.");
	    LOGGER.info("2. Verify upnp version details");
	    LOGGER.info("3. Verify the CcspXdnsSsp process is up and running on the device");
	    LOGGER.info("4.kill and verify the CcspXdnsSsp process");
	    LOGGER.info(
		    "5. Verify log 'Resetting process CcspXdnsSsp' is available in if it is DSL store logs into /rdklogs/logs/systemd_processRestart.log  if not store logs into /rdklogs/logs/SelfHeal.txt.0");
	    LOGGER.info("6. Set and verify the XDNS status");
	    LOGGER.info("POST-CONDITION 1 : Verify the xdns status is disabled");
	    LOGGER.info("#######################################################################################");

	    /**
	     * Step 1 : Reboot the gateway device
	     */
	    stepNum = "S" + stepNumber;
	    status = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : VERIFY DEVICE REBOOT IS SUCCESSFUL");
	    LOGGER.info("STEP " + stepNumber + " : ACTION : EXECUTE COMMAND :/sbin/reboot");
	    LOGGER.info("STEP " + stepNumber + " : EXPECTED : DEVICE SHOULD COMEUP");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "DEVICE REBOOT OPERATION FAILED";
	    status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv)
		    && BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
	    if (status) {
		LOGGER.info("STEP " + stepNumber + " : ACTUAL : DEVICE REBOOTED SUCCESSFULLY");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    /**
	     * Step 2 : Verify upnp version details
	     */
	    stepNumber++;
	    stepNum = "S" + stepNumber;
	    status = false;
	    errorMessage = null;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : VERIFY UPNP VERSION DETAILS");
	    LOGGER.info("STEP " + stepNumber + " : ACTION : EXECUTE COMMAND: miniupnpd --version");
	    LOGGER.info("STEP " + stepNumber + " : EXPECTED : UPNP VERSION DETAIL MUST BE 2.1 OR GREATER THAN 2.1");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Current image is not running with latest upnp version";

	    response = tapEnv.executeCommandUsingSsh(device, BroadBandTestConstants.CMD_GET_UPNP_VERSION);
	    response = CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_GET_UPNP_VERSION);
	    LOGGER.info("Obtained current upnp version is : " + response);

	    
	    float latestUpnpVersion = Float.parseFloat(BroadbandPropertyFileHandler.getValueForCurrentUPNPVersion(device));
	    if (CommonMethods.isNotNull(response)) {
		Float version = Float.parseFloat(response);
		status = version >= latestUpnpVersion;
		if (!status) {
		    errorMessage = errorMessage + " current upnp version is  : " + response;
		}
	    } else {
		errorMessage = "Failed to get the upnp version details using miniupnpd --version command";
	    }

	    if (status) {
		LOGGER.info("STEP " + stepNumber + " : ACTUAL : CURRENT IMAGE IS RUNNING WITH LATEST UPNP VERSION "
			+ response);
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * Step 3 : Verify the CcspXdnsSsp process is up and running on the device.
	     */
	    stepNumber++;
	    stepNum = "S" + stepNumber;
	    status = false;
	    errorMessage = null;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + " : DESCRIPTION : VERIFY CcspXdnsSsp PROCESS IS UP AND RUNNING ON THE DEVICE.");
	    LOGGER.info("STEP " + stepNumber + " : ACTION : EXECUTE COMMAND: ps | grep CcspXdnsSsp");
	    LOGGER.info("STEP " + stepNumber + " : EXPECTED : EXPECTED PROCESS SHOULD BE UP AND RUNNING ON THE DEVICE");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "EXPECTED PROCESS IS NOT UP AND RUNNING ON THE DEVICE";
	    response = CommonMethods.getPidOfProcess(device, tapEnv,
		    BroadBandCommandConstants.POROCESS_NAME_CCSPXDNSSSP);
	    status = CommonMethods.isNotNull(response);
	    if (status) {
		LOGGER.info(
			"STEP " + stepNumber + " : ACTUAL :EXPECTED PROCESS SHOULD BE UP AND RUNNING ON THE DEVICE");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    /**
	     * Step 4: kill and verify the CcspXdnsSsp process
	     */
	    stepNumber++;
	    stepNum = "S" + stepNumber;
	    status = false;
	    errorMessage = null;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : KILL AND VERIFY THE  CcspXdnsSsp PROCESS");
	    LOGGER.info("STEP " + stepNumber + " : ACTION : EXECUTE COMMAND : kill -11 <PID of CcspXdnsSsp process");
	    LOGGER.info("STEP " + stepNumber + " : EXPECTED : CcspXdnsSsp PROCESS SHOULD BE TERMINATED OR KILLED");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "UNABLE TO KILL THE PROCESS";
	    status = BroadBandSystemUtils.killAndVerifyProcessRestarted(device, tapEnv,
		    BroadBandCommandConstants.POROCESS_NAME_CCSPXDNSSSP);
	    if (status) {
		LOGGER.info("STEP " + stepNumber + " : ACTUAL : CcspXdnsSsp PROCESS KILLED SUCCESSFULLY");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * Step 5 : Verify the CcspXdnsSsp process available in /rdklogs/logs/systemd_processRestart.log for arm and
	     * DSL devices and for other devices it is available in /rdklogs/logs/SelfHeal.txt.0 .
	     */
	    stepNumber++;
	    stepNum = "S" + stepNumber;
	    status = false;
	    errorMessage = null;
	    String command = null;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + " : DESCRIPTION : Verify the CcspXdnsSsp process available in /rdklogs/logs/systemd_processRestart.log for arm and DSL devices  and for other devices it is available in /rdklogs/logs/SelfHeal.txt.0");
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTION : EXECUTE COMMAND: FOR arm and DSL DEVICES : cat /rdklogs/logs/systemd_processRestart.log |grep -i CcspXdnsSsp, and for other DEVICES:cat /rdklogs/logs/SelfHeal.txt.0 |grep -i CcspXdnsSsp ");
	    LOGGER.info("STEP " + stepNumber
		    + " : EXPECTED : EXPECTED LOG: FOR other DEVICES :'Resetting process CcspXdnsSsp' SHOULD BE PRESENT IN /rdklogs/logs/SelfHeal.txt.0,FOR other devices AND DSL DEVICES:'Stopping/Restarting CcspXdnsSsp' SHOULD BE PRESENT IN /rdklogs/logs/systemd_processRestart.log");
	    LOGGER.info("#######################################################################################");
	    errorMessage = isSystemdPlatforms
		    ? "EXPECTED LOG 'Stopping/Restarting CcspXdnsSsp' IS NOT PRESENT IN /rdklogs/logs/systemd_processRestart.log"
		    : "EXPECTED LOG 'Resetting process CcspXdnsSsp' IS NOT PRESENT IN /rdklogs/logs/SelfHeal.txt.0";
	    if (isSystemdPlatforms) {
		command = BroadBandCommonUtils.concatStringUsingStringBuffer(LinuxCommandConstants.COMMAND_CAT,
			BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandCommandConstants.FILE_SYSTEMD_PROCESS,
			BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandTestConstants.SYMBOL_PIPE,
			BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandTestConstants.GREP_COMMAND,
			BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandCommandConstants.POROCESS_NAME_CCSPXDNSSSP,
			BroadBandTestConstants.TEXT_DOUBLE_QUOTE);
	    } else {
		command = BroadBandCommonUtils.concatStringUsingStringBuffer(LinuxCommandConstants.COMMAND_CAT,
			BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandCommandConstants.FILE_SELFHEAL_LOG,
			BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandTestConstants.SYMBOL_PIPE,
			BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandTestConstants.GREP_COMMAND,
			BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandCommandConstants.POROCESS_NAME_CCSPXDNSSSP,
			BroadBandTestConstants.TEXT_DOUBLE_QUOTE);
	    }
	    response = tapEnv.executeCommandUsingSsh(device, command);
	    status = (CommonMethods.isNotNull(response) && isSystemdPlatforms
		    ? CommonUtils.patternSearchFromTargetString(response,
			    BroadBandTestConstants.TEXT_RESTARTING_CCSPXDNSSSP)
		    : CommonUtils.patternSearchFromTargetString(response,
			    BroadBandTestConstants.TEXT_RESETTING_CCSPXDNSSSP));
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : EXPECTED LOG for starting the 'CcspXdnsSsp' process IS PRESENT IN the logs.");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * Step 6: Set and verify the XDNS status
	     */
	    stepNumber++;
	    stepNum = "S" + stepNumber;
	    setAndVerifyXdnsFeature(device, testCaseId, stepNum, true);
	} catch (Exception e) {
	    errorMessage = e.getMessage();
	    LOGGER.error("EXCEPTION OCCURRED WHILE VERIFYING XDNS IN PROCESS MONITOR AFTER REBOOT :" + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    true);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    BroadBandPostConditionUtils.executePostConditionToDisableXdnsStatus(device, tapEnv,
		    BroadBandTestConstants.CONSTANT_1);
	    LOGGER.info("########################### ENDING POST CONFIGURATION ####################################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-CCSP-XDNS-5001");
    }
    
    /**
     * Common step to set and verify the Xdns Feature using webpa param Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS
     * 
     * @param device
     * @param testCaseId
     * @param stepNumber
     * @param valueToSet
     *            boolean value to enable or disable the xdns feature
     * @refactor yamini.s
     */

    private void setAndVerifyXdnsFeature(Dut device, String testCaseId, String stepNumber, boolean valueToSet) {
	/**
	 * STEP: Enable/Disable and verify the XDNS feature using webpa param
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS
	 * <li>EXPECTED: XDNS feature should be enabled/disabled using webpa param
	 * 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'
	 */
	// boolean variable to store the status
	boolean status = false;
	// XDNS Value
	String xdnsValue = valueToSet ? BroadBandTestConstants.TRUE : BroadBandTestConstants.FALSE;
	// Error message
	String errorMessage = valueToSet
		? "Failed to enable the XDNS feature using webpa param 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'"
		: "Failed to disable the XDNS feature using webpa param 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'";
	// String to store the step description
	String stepDescription = valueToSet
		? "Enable and verify the XDNS feature using webpa param 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'"
		: "Disable and verify the XDNS feature using webpa param 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'";
	// String to store the expected result
	String expectedResult = valueToSet
		? "XDNS feature should be enabled using webpa param 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'"
		: "XDNS feature should be disabled using webpa param 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'";
	// String to store the success message
	String successMessage = valueToSet
		? "XDNS feature enabled sucessfully using webpa param 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'"
		: "XDNS feature disabled sucessfully using webpa param 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'";
	// String to store the action
	String action = valueToSet
		? "Enable the XDNS feature using webpa param 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'"
		: "Disable the XDNS feature using webpa param 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'";

	LOGGER.info("#######################################################################################");
	LOGGER.info("STEP :  " + stepNumber + " : DESCRIPTION : " + stepDescription);
	LOGGER.info("STEP :  " + stepNumber + " : ACTION : " + action);
	LOGGER.info("STEP :  " + stepNumber + " : EXPECTED: " + expectedResult);
	LOGGER.info("#######################################################################################");
	status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_XDNS_FEATURE_STATUS, BroadBandTestConstants.CONSTANT_3, xdnsValue,
		BroadBandTestConstants.THREE_MINUTES, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	LOGGER.info("STEP " + stepNumber + " - ACTUAL: " + (status ? successMessage : errorMessage));
	tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, valueToSet);
    }

    /**
     * Test to validate the private 24/5ghz_Verify BSSID remains same after Factory Reset
     * 
     * <li>1. Execute the command to get 2.4 GHz private bssid from gateway</li>
     * <li>2. Execute webpa query to get 2.4 GHz private bssid via webpa</li>
     * <li>3. Execute the command to get 5 GHz private bssid from gateway</li>
     * <li>4. Execute webpa query to get 5 GHz private bssid via webpa</li>
     * <li>5. Perform factory reset on the unit via WebPa</li>
     * <li>6. Execute the command to get 2.4 GHz private bssid from gateway</li>
     * <li>7. Execute webpa query to get 2.4 GHz private bssid via webpa</li>
     * <li>8. Execute the command to get 5 GHz private bssid from gateway</li>
     * <li>9. Execute webpa query to get 5 GHz private bssid via webpa</li>
     * <li>10. Validate if arm console is accessible</li>
     * 
     * @param device
     *            Instance {@link Dut}
     * 
     * @author Sathurya_R
     * @Refactor Rakesh C N
     * 
     */

    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
	    BroadBandTestGroup.SYSTEM })
    @TestDetails(testUID = "TC-RDKB-BSSID-PERSIST-1001")
    public void testVerifyBssidPersistFactoryReset(Dut device) {

	// Variable declaration begins
	String testCaseId = "";
	String stepNum = null;
	int stepNumber = 1;
	String errorMessage = "";
	boolean status = false;
	String response = null;
	// Variable declaration ends

	testCaseId = "TC-RDKB-BSSID-PERSIST-001";

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-BSSID-PERSIST-1001");
	LOGGER.info("TEST DESCRIPTION: Validate the private 24/5ghz_Verify BSSID remains same after Factory Reset");

	LOGGER.info("TEST STEPS : ");

	LOGGER.info("1. Execute the command to get 2.4 GHz private bssid from gateway ");
	LOGGER.info("2. Execute webpa query to get 2.4 GHz private bssid via webpa ");
	LOGGER.info("3. Execute the command to get 5 GHz private bssid from gateway ");
	LOGGER.info("4. Execute webpa query to get 5 GHz private bssid via webpa ");
	LOGGER.info("5. Perform factory reset on the unit via WebPa ");
	LOGGER.info("6. Execute the command to get 2.4 GHz private bssid from gateway ");
	LOGGER.info("7. Execute webpa query to get 2.4 GHz private bssid via webpa ");
	LOGGER.info("8. Execute the command to get 5 GHz private bssid from gateway ");
	LOGGER.info("9. Execute webpa query to get 5 GHz private bssid via webpa ");
	LOGGER.info("10. Validate if arm console is accessible ");
	LOGGER.info("#######################################################################################");

	try {
	    stepNum = "S" + stepNumber;
	    errorMessage = "Attempt to execute the command was not successful";
	    status = false;
	    String bssid_device = null;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Execute the command to get 2.4 GHz private bssid from gateway   ");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION:  Execute the command 'iwconfig ath0 |iwconfig wifi2_0 |wl -i wl0 status' to get bssid from gateway  ");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED:  Valid BSSID should be returned ");
	    LOGGER.info("#####################################################################################");

	    bssid_device = BroadBandWiFiUtils.getBssidFromGateway(device, tapEnv, WiFiFrequencyBand.WIFI_BAND_2_GHZ);
	    status = CommonMethods.isNotNull(bssid_device) && CommonMethods.isMacValid(bssid_device);
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Attempt to execute the command is successful");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage + " Resposne: " + response);
	    }

	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    ++stepNumber;
	    stepNum = "S" + stepNumber;
	    errorMessage = "The wifi bssid from device and webpa does not match";
	    String bssid_webpa = null;
	    String bssidBeforeReboot24 = null;

	    status = false;
	    LOGGER.info("****************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Execute webpa query to get 2.4 GHz private bssid via webpa   ");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION:  Execute the webpa for the object 'Device.WiFi.SSID.10001.BSSID'  ");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED: Valid BSSID should be returned ");
	    LOGGER.info("****************************************************************************************");

	    bssid_webpa = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_BSSID_FOR_2_4GHZ);
	    status = CommonMethods.isNotNull(bssid_webpa) && BroadBandCommonUtils
		    .compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON, bssid_webpa, bssid_device);
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : The wifi bssid from device and webpa match");
		bssidBeforeReboot24 = bssid_device;
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage + " Device: " + bssid_device
			+ " Webpa: " + bssid_webpa);
	    }

	    LOGGER.info("****************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    ++stepNumber;
	    stepNum = "S" + stepNumber;
	    errorMessage = "Attempt to execute the command was not successful";
	    status = false;
	    bssid_device = null;
	    LOGGER.info("****************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Execute the command to get 5 GHz private bssid from gateway   ");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION:  Execute the command 'iwconfig ath1 |iwconfig wifi0_0 |wl -i wl1 status' to get bssid from gateway  ");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED:  Valid BSSID should be returned ");
	    LOGGER.info("****************************************************************************************");

	    bssid_device = BroadBandWiFiUtils.getBssidFromGateway(device, tapEnv, WiFiFrequencyBand.WIFI_BAND_5_GHZ);
	    status = CommonMethods.isNotNull(bssid_device) && CommonMethods.isMacValid(bssid_device);
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Attempt to execute the command is successful");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage + " Resposne: " + response);
	    }

	    LOGGER.info("****************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    ++stepNumber;
	    stepNum = "S" + stepNumber;
	    errorMessage = "The wifi bssid from device and webpa does not match";
	    bssid_webpa = null;
	    String bssidBeforeReboot5 = null;

	    status = false;
	    LOGGER.info("****************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Execute webpa query to get 5 GHz private bssid via webpa   ");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION:  Execute the webpa for the object 'Device.WiFi.SSID.10101.BSSID'  ");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED: Valid BSSID should be returned ");
	    LOGGER.info("****************************************************************************************");

	    bssid_webpa = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_BSSID_FOR_5GHZ);
	    status = CommonMethods.isNotNull(bssid_webpa) && BroadBandCommonUtils
		    .compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON, bssid_webpa, bssid_device);
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : The wifi bssid from device and webpa match");
		bssidBeforeReboot5 = bssid_device;
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage + " Device: " + bssid_device
			+ " Webpa: " + bssid_webpa);
	    }

	    LOGGER.info("****************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    ++stepNumber;
	    stepNum = "S" + stepNumber;
	    errorMessage = "Attempt to perform factory reset via webpa has failed";
	    status = false;
	    LOGGER.info("****************************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Perform factory reset on the unit via WebPa ");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : a) Execute WebPa SET command on the object Device.X_CISCO_COM_DeviceControl.FactoryReset ");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : The parameter should get set successfully and return a 200 success response");
	    LOGGER.info("****************************************************************************************");

	    status = BroadBandCommonUtils.performFactoryResetWebPa(tapEnv, device);

	    if (status) {
		LOGGER.info("STEP " + stepNumber + " : ACTUAL : Attempt to factory reset via WEBPA is successfull!");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    ++stepNumber;
	    stepNum = "S" + stepNumber;
	    errorMessage = "Attempt to execute the command was not successful";
	    status = false;
	    bssid_device = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Execute the command to get 2.4 GHz private bssid from gateway   ");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION:  Execute the command 'iwconfig ath0 |iwconfig wifi2_0 |wl -i wl0 status' to get bssid from gateway  ");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED:  Valid BSSID should be returned ");
	    LOGGER.info("**********************************************************************************");

	    bssid_device = BroadBandWiFiUtils.getBssidFromGateway(device, tapEnv, WiFiFrequencyBand.WIFI_BAND_2_GHZ);
	    status = CommonMethods.isNotNull(bssid_device) && CommonMethods.isMacValid(bssid_device);
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Attempt to execute the command is successful");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage + " Resposne: " + response);
	    }

	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    ++stepNumber;
	    stepNum = "S" + stepNumber;
	    errorMessage = "The wifi bssid from device and webpa does not match";
	    bssid_webpa = null;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Execute webpa query to get 2.4 GHz private bssid via webpa   ");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION:  Execute the webpa for the object 'Device.WiFi.SSID.10001.BSSID'  ");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED: Valid BSSID should be returned ");
	    LOGGER.info("**********************************************************************************");

	    bssid_webpa = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_BSSID_FOR_2_4GHZ);
	    status = CommonMethods.isNotNull(bssid_webpa)
		    && BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON, bssid_webpa,
			    bssid_device)
		    && BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
			    bssidBeforeReboot24, bssid_device);
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : The wifi bssid from device ,webpa and pre-reboot match");
		bssidBeforeReboot24 = bssid_device;
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage + " Device: " + bssid_device
			+ " Webpa: " + bssid_webpa + " before factory reset: " + bssidBeforeReboot24);
	    }

	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    ++stepNumber;
	    stepNum = "S" + stepNumber;
	    errorMessage = "Attempt to execute the command was not successful";
	    status = false;
	    bssid_device = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Execute the command to get 5 GHz private bssid from gateway   ");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION:  Execute the command 'iwconfig ath1 |iwconfig wifi0_0 |wl -i wl1 status' to get bssid from gateway  ");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED:  Valid BSSID should be returned ");
	    LOGGER.info("**********************************************************************************");

	    bssid_device = BroadBandWiFiUtils.getBssidFromGateway(device, tapEnv, WiFiFrequencyBand.WIFI_BAND_5_GHZ);
	    status = CommonMethods.isNotNull(bssid_device) && CommonMethods.isMacValid(bssid_device);
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Attempt to execute the command is successful");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage + " Resposne: " + response);
	    }

	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    ++stepNumber;
	    stepNum = "S" + stepNumber;
	    errorMessage = "The wifi bssid from device and webpa does not match";
	    bssid_webpa = null;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Execute webpa query to get 5 GHz private bssid via webpa   ");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION:  Execute the webpa for the object 'Device.WiFi.SSID.10101.BSSID'  ");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED: Valid BSSID should be returned ");
	    LOGGER.info("**********************************************************************************");

	    bssid_webpa = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_BSSID_FOR_5GHZ);
	    status = CommonMethods.isNotNull(bssid_webpa)
		    && BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON, bssid_webpa,
			    bssid_device)
		    && BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
			    bssidBeforeReboot5, bssid_device);
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : The wifi bssid from device ,webpa and pre-reboot match");
		bssidBeforeReboot5 = bssid_device;
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage + " Device: " + bssid_device
			+ " Webpa: " + bssid_webpa + " before factory reset: " + bssidBeforeReboot5);
	    }

	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    ++stepNumber;
	    stepNum = "S" + stepNumber;
	    errorMessage = "The arm console is not accessible";
	    bssid_webpa = null;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Validate if arm console is accessible ");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION:  Execute the command sudo stbsshv6 <ecm_ip> 'echo test_connection' from jump server  ");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED: The arm console should be accessible");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isSTBAccessible(device);

	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : The arm console is accessible");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }

	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    true);
	}
    }
}
