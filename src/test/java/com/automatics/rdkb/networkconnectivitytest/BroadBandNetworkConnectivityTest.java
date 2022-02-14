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
package com.automatics.rdkb.networkconnectivitytest;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.codehaus.jettison.json.JSONObject;
import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.networkconnectivity.BroadBandNetworkConnectivityUtils;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpMib;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.rdkb.utils.telemetry.BroadBandTelemetryUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.snmp.SnmpDataType;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.AutomaticsPropertyUtility;
import com.automatics.utils.CommonMethods;
import com.automatics.webpa.WebPaServerResponse;

/**
 * 
 * Test class for testing the network Connectivity and checking the Ping default values.
 * 
 * @author Joseph_Maduram
 * @Refactor Govardhan
 *
 */
public class BroadBandNetworkConnectivityTest extends AutomaticsTestBase {
    /**
     * 
     * 
     * TC-RDKB-NW-CONNECTIVITY-1001: Verify the ping related default values in the device after factory resetting by
     * Webpa.
     *
     * <ol>
     * <li>S1)Verification of Number of pings per server using the TR181
     * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_NumPingsPerServer to get the default
     * NumPingperServer</li>
     * <li>S2)Verification of Minimum Number of ping server using the TR181
     * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_MinNumPingServer to get the default
     * MinNumPingServer</li>
     * <li>S3)Verification of ping Interval using the TR181
     * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval to get the default pingInterval</li>
     * <li>S4)Verification of ping response wait time using the TR181
     * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_PingRespWaitTime to get the default
     * PingRespWaitTime</li>
     * <li>S5)Verification of usage compute window using the TR181
     * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_UsageComputeWindow to get the default
     * UsageComputeWindow value</li>
     * <li>S6)Verification of AvgCPUThreshold using the TR181
     * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_AvgCPUThreshold to get default the
     * AvgCPUThreshold</li>
     * <li>S7)Verification of AvgMemoryThreshold using the TR181
     * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_AvgMemoryThreshold to get the default
     * AvgMemoryThreshold</li>
     * <li>S8)Verification of MaxRebootCount using the TR181
     * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_MaxRebootCount to get the default MaxRebootCount</li>
     * <li>S9)Verification of MaxResetCount using the TR181
     * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_MaxResetCount to get the default MaxResetCount
     * value</li>
     * <li>S10)Verification of TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval to set the
     * pingInterval as 14</li>
     * <li>S11)Verification of TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval to set the
     * Invalid pingInterval as 1441</li>
     * <li>S12) Verification of TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval to set
     * the valid pingInterval between 14 and 1440</li>
     * </ol>
     *
     * @author Joseph_Maduram
     * @Refactor Govardhan
     * 
     * @param device
     *            {@link Dut}
     */

    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    BroadBandTestGroup.WEBPA }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-NW-CONNECTIVITY-1001")
    public void testNetworkConnectivityForDefaultValuesUsingWebpa(Dut device) {
	// String to store the test case ID
	String testId = "TC-RDKB-NW-CONNECTIVITY-101";
	// String to store the test step number
	String testStepNumber = "s1";
	// String to store the error message
	String errorMessage = null;
	// stores the test status
	boolean status = false;
	// Stores the boolean value
	boolean isFactoryReset = false;
	// stores the response
	String response = null;
	// stores the webPaServer Response
	WebPaServerResponse webPaServerResponse = null;

	// stores default maxResetCount value
	String defaultMaxResetCount = BroadBandTestConstants.STRING_VALUE_THREE;
	try {

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("PRE-CONDITION:DESCRIPTION : Factory reset the device to check default values");
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("PRE-CONDITION : ACTION : performing factory reset by webpa");
	    LOGGER.info("PRE-CONDITION : EXPECTED : The device should get factory resetted by webpa");
	    if (!BroadBandCommonUtils.performFactoryResetWebPa(tapEnv, device)) {
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "Factory Resetting the device failed");
	    }
	    isFactoryReset = true;
	    LOGGER.info("PRE-CONDITION : ACTUAL: Factory Reset is successful");
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-NW-CONNECTIVITY-1001");
	    LOGGER.info(
		    "TEST DESCRIPTION: Verify the ping related default values in the device after factory resetting by  WebPA.");

	    /**
	     * STEP 1:Verification of Number of pings per server using the TR181
	     * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_NumPingsPerServer to get the default
	     * NumPingperServer
	     * 
	     */
	    tapEnv.waitTill(BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS);
	    testStepNumber = "s1";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 1:Verification of Number of pings per server using the TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_NumPingsPerServer to get the default NumPingperServer");
	    LOGGER.info("EXPECTED-NumPingperServer should have the default value as 3");
	    LOGGER.info("#####################################################################################");
	    response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.NUM_PINGS_PER_SERVER);
	    LOGGER.info("NumPingPerServer retrieved using WebPa = " + response);
	    status = CommonMethods.isNotNull(response)
		    && response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_THREE);
	    errorMessage = "Unable to verify  the NumPingsPerServer using WebPA command on TR181 parameter'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_NumPingsPerServer' -Expected value:"
		    + BroadBandTestConstants.STRING_VALUE_THREE + "|Actual Value:" + response;
	    LOGGER.info("S1 ACTUAL : " + (status
		    ? "Successfully verified the default NumPingperServer 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_NumPingsPerServer' using WebPA command.-Expected value:"
			    + BroadBandTestConstants.STRING_VALUE_THREE + "|Actual Value:" + response
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 2:Verification of Minimum Number of ping server using the TR181
	     * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_MinNumPingServer to get the default
	     * MinNumPingServer
	     */

	    testStepNumber = "s2";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 2:Verification of Minimum Number of ping server using the TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_MinNumPingServer to get the default MinNumPingServer");
	    LOGGER.info("EXPECTED-MinNumPingServer should have the default value as 1");
	    LOGGER.info("#####################################################################################");
	    response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.MIN_NUM_PINGS_SERVER);
	    LOGGER.info("minNumPingServer retrieved using WebPa = " + response);
	    status = CommonMethods.isNotNull(response)
		    && response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_ONE);
	    errorMessage = "Unable to verify the MinNumPingServer using WebPA command on TR181 parameter 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_MinNumPingServer' -Expected value:"
		    + BroadBandTestConstants.STRING_VALUE_ONE + "|Actual Value:" + response;
	    LOGGER.info("S2 ACTUAL : " + (status
		    ? "Successfully verified the default MinNumPingServer 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_MinNumPingServer' using WebPA command.-Expected value:"
			    + BroadBandTestConstants.STRING_VALUE_ONE + "|Actual Value:" + response
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 3:Verification of ping Interval using the TR181
	     * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval to get the default pingInterval
	     */

	    testStepNumber = "s3";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 3:Verification of ping Interval using the TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval  to get the default pingInterval");
	    LOGGER.info("EXPECTED-pingInterval should have the default value as 60");
	    LOGGER.info("#####################################################################################");
	    response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.PING_INTERVAL);
	    LOGGER.info("pingInterval retrieved using WebPa = " + response);
	    status = CommonMethods.isNotNull(response)
		    && response.equalsIgnoreCase(BroadBandTestConstants.CONSTANT_DEFAULT_PING_INTERVAL);
	    errorMessage = "Unable to verify the pingInterval  using WebPA command on TR181 parameter 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval ' -Expected value:"
		    + BroadBandTestConstants.CONSTANT_DEFAULT_PING_INTERVAL + "|Actual Value:" + response;
	    LOGGER.info("S3 ACTUAL : " + (status
		    ? "Successfully verified the default pingInterval  'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval' using WebPA command. -Expected value:"
			    + BroadBandTestConstants.CONSTANT_DEFAULT_PING_INTERVAL + "|Actual Value:" + response
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 4:Verification of ping response wait time using the TR181
	     * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_PingRespWaitTime to get the default
	     * PingRespWaitTime
	     * 
	     */

	    testStepNumber = "s4";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 4:Verification of ping response wait time using the TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_PingRespWaitTime to get the default PingRespWaitTime");
	    LOGGER.info("EXPECTED-PingRespWaitTime should have the default value as 1000");
	    LOGGER.info("#####################################################################################");
	    response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.PING_RESP_WAIT_TIME);
	    LOGGER.info("pingRespWaitTime retrieved using WebPa = " + response);
	    status = CommonMethods.isNotNull(response)
		    && response.equalsIgnoreCase(BroadBandTestConstants.CONSTANT_PING_RESP_WAIT_TIME);
	    errorMessage = "Unable to verify the pingRespWaitTime using WebPA command on TR181 parameter  'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_PingRespWaitTime' -Expected value:"
		    + BroadBandTestConstants.CONSTANT_PING_RESP_WAIT_TIME + "|Actual Value:" + response;
	    LOGGER.info("S4 ACTUAL : " + (status
		    ? "Successfully verified the default PingRespWaitTime 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_PingRespWaitTime' using WebPA command.-Expected value:"
			    + BroadBandTestConstants.CONSTANT_PING_RESP_WAIT_TIME + "|Actual Value:" + response
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 5:Verification of usage compute window using the TR181
	     * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_UsageComputeWindow to get the default
	     * UsageComputeWindow value
	     */

	    testStepNumber = "s5";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 5:Verification of usage compute window using the TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_UsageComputeWindow  to get the default UsageComputeWindow value");
	    LOGGER.info("EXPECTED-UsageComputeWindow should have the default value as 15");
	    LOGGER.info("#####################################################################################");
	    response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.USAGE_COMPUTE_WINDOW);
	    LOGGER.info("UsageComputeWindow retrieved using WebPa = " + response);
	    status = CommonMethods.isNotNull(response)
		    && response.equalsIgnoreCase(BroadBandTestConstants.CONSTANT_RESOURCE_USAGE_COMPUTE_WINDOW);
	    errorMessage = "Unable to verify the UsageComputeWindow  using WebPA command on TR181 parameter 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_UsageComputeWindow'  -Expected value:"
		    + BroadBandTestConstants.CONSTANT_RESOURCE_USAGE_COMPUTE_WINDOW + "|Actual Value:" + response;
	    LOGGER.info("S5 ACTUAL : " + (status
		    ? "Successfully verified the default UsageComputeWindow  'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_UsageComputeWindow' using WebPA command.-Expected value:"
			    + BroadBandTestConstants.CONSTANT_RESOURCE_USAGE_COMPUTE_WINDOW + "|Actual Value:"
			    + response
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 6:Verification of AvgCPUThreshold using the TR181
	     * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_AvgCPUThreshold to get default the
	     * AvgCPUThreshold
	     */

	    testStepNumber = "s6";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 6:Verification of AvgCPUThreshold using the TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_AvgCPUThreshold  to get the default AvgCPUThreshold ");
	    LOGGER.info("EXPECTED-AvgCPUThreshold should have the default value as 100");
	    LOGGER.info("#####################################################################################");
	    response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.AVG_CPU_THRESHOLD);
	    LOGGER.info("AvgCPUThreshold retrieved using WebPa = " + response);
	    status = CommonMethods.isNotNull(response)
		    && response.equalsIgnoreCase(BroadBandTestConstants.CONSTANT_AVG_CPU_THRESHOLD);
	    errorMessage = "Unable to verify the avgCPUThreshold using WebPA command on TR181 parameter 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_AvgCPUThreshold'-Expected value:"
		    + BroadBandTestConstants.CONSTANT_AVG_CPU_THRESHOLD + "|Actual Value:" + response;
	    LOGGER.info("S6 ACTUAL : " + (status
		    ? "Successfully verified the default AvgCPUThreshold  'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_AvgCPUThreshold' using WebPA command.-Expected value:"
			    + BroadBandTestConstants.CONSTANT_AVG_CPU_THRESHOLD + "|Actual Value:" + response
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 7:Verification of AvgMemoryThreshold using the TR181
	     * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_AvgMemoryThreshold to get the default
	     * AvgMemoryThreshold
	     */

	    testStepNumber = "s7";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 7:Verification of AvgMemoryThreshold using the TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_AvgMemoryThreshold  to get the default AvgMemoryThreshold");
	    LOGGER.info("EXPECTED-AvgMemoryThreshold should have the default value as 100");
	    LOGGER.info("#####################################################################################");
	    response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.AVG_MEMORY_THRESHOLD);
	    LOGGER.info("AvgMemoryThreshold retrieved using WebPa = " + response);
	    status = CommonMethods.isNotNull(response)
		    && response.equalsIgnoreCase(BroadBandTestConstants.CONSTANT_AVG_MEMORY_THRESHOLD);
	    errorMessage = "Unable to verify the AvgMemoryThreshold using WebPA command on TR181 parameter 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_AvgMemoryThreshold' -Expected value:"
		    + BroadBandTestConstants.CONSTANT_AVG_MEMORY_THRESHOLD + "|Actual Value:" + response;
	    LOGGER.info("S7 ACTUAL : " + (status
		    ? "Successfully verified the default AvgMemoryThreshold  'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_AvgMemoryThreshold' using WebPA command. -Expected value:"
			    + BroadBandTestConstants.CONSTANT_AVG_MEMORY_THRESHOLD + "|Actual Value:" + response
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 8: Verification of MaxRebootCount using the TR181
	     * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_MaxRebootCount to get the default
	     * MaxRebootCount value
	     */

	    testStepNumber = "s8";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 8:Verification of MaxRebootCount using the TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_MaxRebootCount  to get the default MaxRebootCount");
	    LOGGER.info("EXPECTED-MaxRebootCount should have the default value as 3");
	    LOGGER.info("#####################################################################################");
	    response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.MAX_REBOOT_COUNT);
	    LOGGER.info("MaxRebootCount retrieved using WebPa = " + response);
	    status = CommonMethods.isNotNull(response)
		    && response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_THREE);
	    errorMessage = "Unable to verify the MaxRebootCount using WebPA command on TR181 parameter'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_MaxRebootCount' -Expected value:"
		    + BroadBandTestConstants.STRING_VALUE_THREE + "|Actual Value:" + response;
	    LOGGER.info("S8 ACTUAL : " + (status
		    ? "Successfully verified the default MaxRebootCount  'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_MaxRebootCount' using WebPA command.-Expected value:"
			    + BroadBandTestConstants.STRING_VALUE_THREE + "|Actual Value:" + response
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 9:Verification of MaxResetCount using the TR181
	     * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_MaxResetCount to get the default
	     * MaxResetCount value
	     */

	    testStepNumber = "s9";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 9:Verification of MaxResetCount using the TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_MaxResetCount  to get the default MaxResetCount value");
	    LOGGER.info(
		    "EXPECTED-MaxResetCount should have the default value as 3 for master ,stable and release build");
	    LOGGER.info("#####################################################################################");
	    response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.MAX_RESET_COUNT);
	    LOGGER.info("MaxResetCount retrieved using WebPa = " + response);
	    defaultMaxResetCount = AutomaticsPropertyUtility
		    .getProperty(BroadBandTestConstants.DEFAULT_MAX_RESET_COUNT);
	    status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(defaultMaxResetCount);
	    errorMessage = "Unable to verify the MaxResetCount using WebPA command on TR181 parameter 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_MaxResetCount' -Expected value:"
		    + defaultMaxResetCount + "|Actual Value:" + response;
	    LOGGER.info("S9 ACTUAL : " + (status
		    ? "Successfully verified the default MaxResetCount  'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_MaxResetCount' using WebPA command.-Expected value:"
			    + defaultMaxResetCount + "|Actual Value:" + response
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 10:Verification of TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval to
	     * set the Invalid pingInterval as 14
	     */

	    testStepNumber = "s10";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 10:Verification of  TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval  to set the pingInterval as 14");
	    LOGGER.info("EXPECTED-Should not be able to set the pinginterval as 14");
	    LOGGER.info("#####################################################################################");
	    errorMessage = "Able to set  the Invalid ping interval for 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval' using WebPA command.";
	    webPaServerResponse = BroadBandWebPaUtils.setWebPaParamAndReturnResp(tapEnv, device,
		    BroadBandWebPaConstants.PING_INTERVAL, BroadBandTestConstants.CONSTANT_PING_INTERVAL_14,
		    BroadBandTestConstants.CONSTANT_2);
	    tapEnv.waitTill(BroadBandTestConstants.FIFTY_SECONDS_IN_MILLIS);
	    status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.INVALID_PARAMETER_VALUE);
	    LOGGER.info(
		    "S10 webPaServerResponse is : " + webPaServerResponse.getMessage() + " & Status is : " + status);
	    LOGGER.info("S10 ACTUAL : " + (status
		    ? " Not able to set the Invalid Ping Interval as 14 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval' using WebPA command."
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 11:Verification of TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval to
	     * set the Invalid pingInterval as 1441
	     */

	    testStepNumber = "s11";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 11:Verification of TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval  to set the pingInterval as 1441");
	    LOGGER.info("EXPECTED-Should not be able to set the pinginterval as 1441");
	    LOGGER.info("#####################################################################################");
	    errorMessage = "Able to set  the Invalid ping interval for 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval' using WebPA command.";

	    webPaServerResponse = BroadBandWebPaUtils.setWebPaParamAndReturnResp(tapEnv, device,
		    BroadBandWebPaConstants.PING_INTERVAL, BroadBandTestConstants.CONSTANT_PING_INTERVAL_1441,
		    BroadBandTestConstants.CONSTANT_2);
	    tapEnv.waitTill(BroadBandTestConstants.FIFTY_SECONDS_IN_MILLIS);
	    status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.INVALID_PARAMETER_VALUE);
	    LOGGER.info(
		    "S11 webPaServerResponse is : " + webPaServerResponse.getMessage() + " & Status is : " + status);
	    LOGGER.info("S11 ACTUAL : " + (status
		    ? " Not able to set the Invalid Ping Interval as 1441 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval' using WebPA command."
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 12:Verification of TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval to
	     * set the valid pingInterval between 14 and 1440
	     */

	    testStepNumber = "s12";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 12:Verification of  TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval  to set the valid pingInterval between 14 and 1440");
	    LOGGER.info("EXPECTED-Should  be able to set the pinginterval between 14 and 1440");
	    LOGGER.info("#####################################################################################");
	    status = BroadBandWiFiUtils.setWebPaParams(device, BroadBandWebPaConstants.PING_INTERVAL,
		    BroadBandTestConstants.CONSTANT_DEFAULT_PING_INTERVAL, BroadBandTestConstants.CONSTANT_2);
	    errorMessage = "Unable to set  the valid ping interval between 14 and 1440 for 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval' using WebPA command.";
	    LOGGER.info("S12 ACTUAL : " + (status
		    ? "able to set the valid Ping Interval between 14 and 1440 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval' using WebPA command."
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
	}

	catch (Exception testException) {
	    errorMessage = "Exception occured while checking the default Values: " + testException.getMessage();
	    LOGGER.error(errorMessage);
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    if (isFactoryReset) {

		LOGGER.info("POST-CONDITION : DESCRIPTION : BEGIN BROAD BAND DEVICE REACTIVATION.");
		LOGGER.info("POST-CONDITION : ACTION : BROAD BAND DEVICE REACTIVATION. ");
		LOGGER.info("POST-CONDITION : EXPECTED : device should get reactivated");
		LOGGER.info("### POST-CONDITION ### BEGIN BROAD BAND DEVICE REACTIVATION.");
		BroadBandWiFiUtils.reactivateBroadBandDeviceWebPa(tapEnv, device);
		LOGGER.info("### POST-CONDITION ### END BROAD BAND DEVICE REACTIVATION.");
		LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	    }
	}

    }

    /**
     * Test case is created as part of COVERAGE AUTOMATION based on the Ping Server to detect network
     * connectivity and Connectivity test using Gateway IP
     * 
     * 
     * TC-RDKB-NW-CONNECTIVITY-1003: Verify whether valid ping servers are set by Webpa and the ping success logs are
     * captured in selfHeallog.txt.0
     *
     * <ol>
     * <li>PRE CONDITION 1:verify Webpa process is up and running</li>
     * <li>PRE CONDITION 2:Check and Set the Ping Interval to 15 mins</li>
     * 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval'using WebPA.</li>
     * <li>S1)Verification of adding valid Ipv4 ping server as in table row 1 by using Webpa POST command on
     * "Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable."</li>
     * <li>S2)Verification of adding valid Ipv4 ping server as in table row 2 by using Webpa POST command on
     * "Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable."</li>
     * <li>S3)Verification of adding valid Ipv4 ping server as in table row 3 by using Webpa POST command on
     * "Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable."</li>
     * <li>S4)Verification of adding valid Ipv6 ping server as in table row 1 by using Webpa POST command on
     * "Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable."</li>
     * <li>S5)Verification of adding valid Ipv6 ping server as in table row 2 by using Webpa POST command on
     * "Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable."</li>
     * <li>S6)Verification of adding valid Ipv6 ping server as in table row 3 by using Webpa POST command on
     * "Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable."</li>
     * <li>S7)verify whether "Connectivity Test is Successfull" message is present in self heallog.txt.0 after setting
     * the Valid servers.</li>
     * <li>S8)verify whether "GW IP Connectivity Test Successfull" message is present in rdklogs/logs/selfheallog.txt.0
     * .</li>
     * <li>S9)verify whether 'Connectivity Test is Successfull' message is present in self heallog.txt.0 after the
     * setting the Valid ping servers in the next cycle for set ping interval time of 15 mins.</li>
     * <li>POST-CONDITION 1: Delete the added ping servers in the Ping Server Table by using WEBPA DELETE command.</li>
     * <li>POST-CONDITION 2: Set the ping interval to default value.</li>
     * </ol>
     *
     * @author Joseph Maduram
     * @Refactor Athira
     * 
     * @param device
     *            {@link Dut}
     */

    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    BroadBandTestGroup.WEBPA }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-NW-CONNECTIVITY-1003")

    public void validPingServerConnectivityTest(Dut device) {
	// Variable Declaration begins
	// String to store the test case ID
	String testId = "TC-RDKB-NW-CONNECTIVITY-103";
	// String to store the test step number
	String testStepNumber = "s1";
	// String to store the error message
	String errorMessage = null;
	// String to store the added table row number
	String tableRowNumber = null;
	// List of String to store the added table row numbers
	List<String> tableRow = new ArrayList<String>();
	// stores the test status
	boolean status = false;
	// string to store the WebPA server response
	WebPaServerResponse webPaServerResponse = null;
	Map<String, List<String>> pingServersTable = new HashMap<String, List<String>>();
	// List of String to store the ping servers
	List<String> pingServers = new ArrayList<String>();
	String ipv4PingServer = null;
	String ipv6PingServer = null;
	String defaultPingInterval = null;
	String response = null;
	// Variable Declaration ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-NW-CONNECTIVITY-1003");
	LOGGER.info(
		"TEST DESCRIPTION:Verify whether valid IP address are set for the  ping servers by Webpa and the ping success logs are captured in selfHeallog.txt.0");
	LOGGER.info("PRE-CONDITION 1: verify Webpa process is up and running");
	LOGGER.info("PRE-CONDITION 2: Check and Set the Ping Interval to 15 mins");
	LOGGER.info(
		"STEP 1 :Verification of adding valid Ipv4 ping server as in table row 1 by using Webpa POST command on Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.");
	LOGGER.info(
		"STEP 2 :Verification of adding valid Ipv4 ping server as in table row 2 by using Webpa POST command on Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.");
	LOGGER.info(
		"STEP 3:Verification of adding valid Ipv4 ping server as in table row 3 by using Webpa POST command on Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.");
	LOGGER.info(
		"STEP 4:Verification of adding valid Ipv6 ping server as in table row 1by using Webpa POST command on Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.");
	LOGGER.info(
		"STEP 5: Verification of adding valid Ipv6 ping server as in table row 2 by using Webpa POST command on Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.");
	LOGGER.info(
		"STEP 6:Verification of adding valid Ipv6 ping server as in table row 3 by using Webpa POST command on Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.");
	LOGGER.info(
		"STEP 7:verify whether Connectivity Test is Successfull message is present in self heallog.txt.0 after setting the Valid servers.");
	LOGGER.info(
		"STEP 8:verify whether GW IP Connectivity Test Successfull message is present in rdklogs/logs/selfheallog.txt.0");
	LOGGER.info(
		"STEP 9:verify whether 'Connectivity Test is Successfull' message  is present in self heallog.txt.0  after the setting the Valid ping servers in the next cycle for set ping interval time of 15 mins.");
	LOGGER.info(
		"POST CONDITION 1:Delete the added ping servers in the Ping Server Table by using WEBPA DELETE command");
	LOGGER.info("POST CONDITION 2:Set the ping interval to default value.");
	LOGGER.info("#######################################################################################");
	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("PRE-CONDITION 1:DESCRIPTION : verify Webpa process is up and running");
	    LOGGER.info(
		    "PRE-CONDITION 1: ACTION : Execute Device.DeviceInfo.SerialNumber and verify Webpa process is up and running'");
	    LOGGER.info("PRE-CONDITION 1: EXPECTED : Webpa process should be up and running");
	    LOGGER.info("#######################################################################################");

	    // If last (third) parameter in verifyWebPaProcessIsUp is set to true, this
	    // command could
	    // potentially run for 8 minutes. As false, it will run no longer than 30
	    // seconds.
	    status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, false);

	    if (status) {
		LOGGER.info("PRE-CONDITION 1: ACTUAL :WEBPA IS UP AND RUNNING - CONTINUING THE EXECUTION");
	    } else {
		LOGGER.error("PRE-CONDITION 1: ACTUAL : Pre condition failed");
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
			+ "WEBPA IS NOT UP AND RUNNING - HENCE BLOCKING THE EXECUTION");
	    }
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("PRE-CONDITION 2:DESCRIPTION : Check and Set the Ping Interval to 15 mins using WebPA.");
	    LOGGER.info(
		    "PRE-CONDITION 2: ACTION : Set the ping Interval to 15 mins using WebPA-'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval'");
	    LOGGER.info("PRE-CONDITION 2: EXPECTED : ping Interval should be set to 15 mins");
	    LOGGER.info("#######################################################################################");
	    defaultPingInterval = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.PING_INTERVAL);
	    if (defaultPingInterval.equalsIgnoreCase(BroadBandTestConstants.CONSTANT_PING_INTERVAL)) {
		status = true;
	    } else {
		status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
			BroadBandWebPaConstants.PING_INTERVAL, BroadBandTestConstants.CONSTANT_2,
			BroadBandTestConstants.CONSTANT_PING_INTERVAL);
	    }
	    if (status) {
		LOGGER.info("PRE-CONDITION 2: ACTUAL : Successfully Set the ping interval to 15 mins");
	    } else {
		LOGGER.error("PRE-CONDITION 2 : ACTUAL : " + errorMessage);
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
			+ "UNABLE TO SET THE PING INTERVAL TO 15 MINS - HENCE BLOCKING THE EXECUTION");
	    }
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

	    /**
	     * STEP 1:Verification of adding valid Ipv4 ping server in table row 1 by using Webpa POST command on
	     * "Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable."
	     * 
	     */
	    testStepNumber = "s1";
	    errorMessage = "Unable to set the  First IPV4 ping server URI with valid Ipv4 ping server using WebPA command.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1:DESCRIPTION: Verification of adding valid Ipv4 ping server in table row 1 by using  Webpa POST command on 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.'");
	    LOGGER.info(
		    "STEP 1:ACTION: Execute Webpa POST command on Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable. to set the table row 1 with valid ipv4 address");
	    LOGGER.info("STEP 1:EXPECTED: should be able to set the IPV4 ping server1  by WebPA");
	    LOGGER.info("**********************************************************************************");
	    ipv4PingServer = BroadBandNetworkConnectivityUtils.retrievePingServerUsingNslookUpForIpv4(device, tapEnv);
	    pingServers.add(ipv4PingServer);
	    pingServersTable.put(BroadBandWebPaConstants.WEBPA_IPV4_PING_SERVER_URI, pingServers);
	    webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
		    BroadBandWebPaConstants.WEBPA_IPV4_PING_ADD_TABLE, pingServersTable);
	    tableRowNumber = webPaServerResponse.getRow();
	    tableRow.add(tableRowNumber);
	    status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
	    if (status) {
		LOGGER.info(
			"STEP 1: ACTUAL : Successfully added the valid Ipv4 ping server as  First IPV4 ping server URI using WebPA command");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 2:Verification of adding valid Ipv4 ping server in table row 2 by using Webpa POST command on
	     * "Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable."
	     * 
	     */

	    testStepNumber = "s2";
	    errorMessage = "Unable to set the  Second IPV4 ping server URI with valid Ipv4 ping server using WebPA command.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 2:DESCRIPTION: Verification of adding valid Ipv4 ping server in table row 2 by using  Webpa POST command on 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.'");
	    LOGGER.info(
		    "STEP 2:ACTION: Execute Webpa POST command on Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable. to set the table row 2 with valid ipv4 address.");
	    LOGGER.info("STEP 2:EXPECTED: should be able to set the IPV4 ping server2 by WebPA");
	    LOGGER.info("**********************************************************************************");
	    pingServersTable.clear();
	    pingServers.clear();
	    pingServers.add(ipv4PingServer);
	    pingServersTable.put(BroadBandWebPaConstants.WEBPA_IPV4_PING_SERVER_URI, pingServers);
	    webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
		    BroadBandWebPaConstants.WEBPA_IPV4_PING_ADD_TABLE, pingServersTable);
	    tableRowNumber = webPaServerResponse.getRow();
	    tableRow.add(tableRowNumber);
	    status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
	    if (status) {
		LOGGER.info(
			"STEP 2: ACTUAL : Successfully added the valid Ipv4 ping server as Second IPV4 ping server URI using WebPA command.");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 3:Verification of adding valid Ipv4 ping server in table row 3 by using Webpa POST command on
	     * "Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable."
	     * 
	     */

	    testStepNumber = "s3";
	    errorMessage = "Unable to set the  Third IPV4 ping server URI with valid Ipv4 ping server using WebPA command.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 3:DESCRIPTION:Verification of adding valid Ipv4 ping server in table row 3 by using  Webpa POST command on 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.'");
	    LOGGER.info(
		    "STEP 3:ACTION: Execute Webpa POST command on Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable. to set the table row 3 with valid ipv4 address");
	    LOGGER.info("STEP 3:EXPECTED: should be able to set the IPV4 ping server3  by WebPA");
	    LOGGER.info("**********************************************************************************");
	    pingServersTable.clear();
	    pingServers.clear();
	    pingServers.add(ipv4PingServer);
	    pingServersTable.put(BroadBandWebPaConstants.WEBPA_IPV4_PING_SERVER_URI, pingServers);
	    webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
		    BroadBandWebPaConstants.WEBPA_IPV4_PING_ADD_TABLE, pingServersTable);
	    tableRowNumber = webPaServerResponse.getRow();
	    tableRow.add(tableRowNumber);
	    status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
	    if (status) {
		LOGGER.info(
			"STEP 3: ACTUAL : Successfully added the valid Ipv4 ping server as  Third IPV4 ping server URI using WebPA command.");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	    /**
	     * STEP 4:Verification of adding valid Ipv6 ping server in table row 1 by using Webpa POST command on
	     * "Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable."
	     * 
	     * 
	     */
	    testStepNumber = "s4";
	    errorMessage = "Unable to set the  First IPV6 ping server URI with valid Ipv6 ping server using WebPA command.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 4:DESCRIPTION:Verification of adding valid Ipv6 ping server  in table row 1 by using  Webpa POST command on 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.'");
	    LOGGER.info(
		    "STEP 4:ACTION: Execute Webpa POST command on Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable. to set the table row 1 with valid ipv6 address");
	    LOGGER.info("STEP 4:EXPECTED: should be able to set the IPV6 ping server1 by WebPA");
	    LOGGER.info("**********************************************************************************");
	    pingServersTable.clear();
	    pingServers.clear();
	    ipv6PingServer = BroadBandNetworkConnectivityUtils.retrievePingServerUsingNslookUpForIpv6(device, tapEnv);
	    LOGGER.info("Ipv6 Ping Server Address = " + ipv6PingServer);
	    pingServers.add(ipv6PingServer);
	    pingServersTable.put(BroadBandWebPaConstants.WEBPA_IPV6_PING_SERVER_URI, pingServers);
	    webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
		    BroadBandWebPaConstants.WEBPA_IPV6_PING_ADD_TABLE, pingServersTable);
	    tableRowNumber = webPaServerResponse.getRow();
	    tableRow.add(tableRowNumber);
	    status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
	    if (status) {
		LOGGER.info(
			"STEP 4: ACTUAL : Successfully added the valid Ipv6 ping server as  First IPV6 ping server URI using WebPA command.");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 5:Verification of adding valid Ipv6 ping server in table row 2 by using Webpa POST command on
	     * "Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable."
	     * 
	     */
	    testStepNumber = "s5";
	    errorMessage = "Unable to set the  Second IPV6 ping server URI with valid Ipv6 ping server using WebPA command.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 5:DESCRIPTION:Verification of adding valid Ipv6 ping server in table row 2 by using  Webpa POST command on 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.'");
	    LOGGER.info(
		    "STEP 5:ACTION: Execute Webpa POST command on Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable. to set the table row 2 with valid ipv6 address");
	    LOGGER.info("STEP 5:EXPECTED: should be able to set the IPV6 ping server2 by WebPA");
	    LOGGER.info("**********************************************************************************");
	    pingServersTable.clear();
	    pingServers.clear();
	    pingServers.add(ipv6PingServer);
	    pingServersTable.put(BroadBandWebPaConstants.WEBPA_IPV6_PING_SERVER_URI, pingServers);
	    webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
		    BroadBandWebPaConstants.WEBPA_IPV6_PING_ADD_TABLE, pingServersTable);
	    tableRowNumber = webPaServerResponse.getRow();
	    tableRow.add(tableRowNumber);
	    status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
	    if (status) {
		LOGGER.info(
			"STEP 5: ACTUAL : Successfully added the valid Ipv6 ping server as second IPV6 ping server URI using WebPA command");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 6:Verification of adding valid Ipv6 ping server in table row 3 by using Webpa POST command on
	     * "Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable."
	     * 
	     */

	    testStepNumber = "s6";
	    errorMessage = "Unable to set the  Third IPV6 ping server URI with valid Ipv6 ping server using WebPA command.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 6:DESCRIPTION:Verification of adding valid Ipv6 ping server  in table row 3 by using  Webpa POST command on 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.'");
	    LOGGER.info(
		    "STEP 6:ACTION: Execute Webpa POST command on Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable. to set the table row 3 with valid ipv6 address");
	    LOGGER.info("STEP 6:EXPECTED: should be able to set the IPV6 ping server3 by WebPA");
	    LOGGER.info("**********************************************************************************");
	    pingServersTable.clear();
	    pingServers.clear();
	    pingServers.add(ipv6PingServer);
	    pingServersTable.put(BroadBandWebPaConstants.WEBPA_IPV6_PING_SERVER_URI, pingServers);
	    webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
		    BroadBandWebPaConstants.WEBPA_IPV6_PING_ADD_TABLE, pingServersTable);
	    tableRowNumber = webPaServerResponse.getRow();
	    tableRow.add(tableRowNumber);
	    status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
	    if (status) {
		LOGGER.info(
			"STEP 6: ACTUAL : Successfully added the valid Ipv6 ping server as third IPV6 ping server URI using WebPA command");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 7 :verify whether "Connectivity Test is Successfull" message is present in self heallog.txt.0 after
	     * the setting the Valid servers.
	     */
	    testStepNumber = "s7";
	    status = false;
	    errorMessage = "Ping server connectivity success message is not logged in the next self heal window";
	    String timeFromFirstLog = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 7:DESCRIPTION:verify whether 'Connectivity Test is Successfull' message  is present in self heallog.txt.0  after the setting the Valid ping servers. ");
	    LOGGER.info(
		    "STEP 7:ACTION: validate whether Connectivity Test is Successfull message  is present in self heallog.txt.0 after setting the required servers");
	    LOGGER.info(
		    "STEP 7:EXPECTED - Valid Ping server connectivity Test success message  should  get logged in the next self heal window");
	    LOGGER.info("**********************************************************************************");
	    try {
		response = BroadBandCommonUtils.searchLogByPolling(tapEnv, device,
			BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.GREP_COMMAND,
				BroadBandTraceConstants.PING_SERVER_SUCCESS_LOG_MESSAGE,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
				BroadBandCommandConstants.FILE_SELFHEAL_LOG, BroadBandTestConstants.SYMBOL_PIPE,
				BroadBandTestConstants.CMD_TAIL_1),
			BroadBandTestConstants.SIXTY_MINUTES_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		status = CommonMethods.isNotNull(response) && CommonUtils.patternSearchFromTargetString(response,
			BroadBandTraceConstants.PING_SERVER_SUCCESS_LOG_MESSAGE.replace(
				BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandTestConstants.EMPTY_STRING));
		if (status) {
		    timeFromFirstLog = CommonMethods.patternFinder(response,
			    BroadBandTestConstants.PATTERN_FINDER_TO_GREP_TIMESTAMP);
		    LOGGER.info("Timestamp from the log is-" + timeFromFirstLog);
		}
	    } catch (Exception e) {
		LOGGER.error(
			"Exception occurred while verifying connectivity message in selfheallog: " + e.getMessage());
	    }
	    if (status) {

		LOGGER.info(
			"STEP 7: ACTUAL : Ping server connectivity success message is logged in the self heal window");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 8 :verify whether "GW IP Connectivity Test Successfull" message is present in
	     * rdklogs/logs/selfheallog.txt.0 .
	     */
	    testStepNumber = "s8";
	    errorMessage = "Gateway connectivity success message is not logged in the self heal window";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 8:DESCRIPTION: verify whether 'GW IP Connectivity Test Successfull' message  is present in rdklogs/logs/selfheallog.txt.0 . ");
	    LOGGER.info(
		    "STEP 8:ACTION:validate whether GW IP Connectivity Test Successfull message  is present in rdklogs/logs/selfheallog.txt.0");
	    LOGGER.info(
		    "STEP 8:EXPECTED - Gateway connectivity success message  should  get logged in the  self heal window");
	    LOGGER.info("**********************************************************************************");
	    response = BroadBandCommonUtils.searchLogByPolling(tapEnv, device,
		    BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.GREP_COMMAND,
			    BroadBandTraceConstants.PING_SERVER_GATEWAY_IP_SUCCESS_LOG_MESSAGE,
			    BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandCommandConstants.FILE_SELFHEAL_LOG,
			    BroadBandTestConstants.SYMBOL_PIPE, BroadBandTestConstants.CMD_TAIL_1),
		    BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
	    status = CommonMethods.isNotNull(response) && CommonUtils.patternSearchFromTargetString(response,
		    BroadBandTraceConstants.PING_SERVER_GATEWAY_IP_SUCCESS_LOG_MESSAGE
			    .replace(BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandTestConstants.EMPTY_STRING));
	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : Gateway connectivity is successful in the self heal window");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 9 :verify whether 'Connectivity Test is Successfull' message is present in self heallog.txt.0 in the
	     * next cycle for set ping interval time of 15 mins
	     */
	    testStepNumber = "s9";
	    errorMessage = "Ping server connectivity success message is not logged in the next cycle self heal window for set ping interval time of 15 mins";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 9:DESCRIPTION: verify whether 'Connectivity Test is Successfull' message  is present in self heallog.txt.0  in the next cycle for set ping interval time of 15 mins.");
	    LOGGER.info(
		    "STEP 9:ACTION: validate whether Connectivity Test is Successfull message  is present in self heallog.txt.0 in the next cycle for set ping interval time of 15 mins");
	    LOGGER.info(
		    "STEP 9:EXPECTED - Valid Ping server connectivity Test success message  should  get logged in the next cycle self heal window");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandCommonUtils.pingTestForSetIntervalTime(device, tapEnv, timeFromFirstLog);
	    if (status) {
		LOGGER.info(
			"STEP 9: ACTUAL : Ping server connectivity success message is logged in the next cycle self heal window for set ping interval time of 15 mins");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	} catch (Exception testException) {
	    errorMessage = "Exception occured while Verifying whether valid IP address are set for the  ping server and the ping success logs are captured in selfHeallog.txt.0 "
		    + testException.getMessage();
	    LOGGER.error(errorMessage);
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
	}
    }

    /**
     * Test Case : Verify whether the Invalid ping servers are set and it's failure messages are logged when the ping
     * fails in selfHeal log and verify whether the Ping failures are captured in telemetry dashboard.
     *
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>PRE-CONDITION 1 : Telemetry profile configuration for gateway configuration.</li>
     * <li>PRE-CONDITION 2 : Check and Set the Ping Interval to 15 mins
     * 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval'using WebPA.</li>
     * <li>STEP 1.Verification of adding Invalid Ipv4 ping server '1.2.3.4' by using Webpa POST command on TR-181
     * parameter'Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.'</li>
     * <li>STEP 2.Verification of adding Invalid Ipv4 ping server '2.3.4.5' by using Webpa POST command on TR-181
     * parameter'Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.'</li>
     * <li>STEP 3.Verification of adding Invalid Ipv4 ping server '3.4.5.6' by using Webpa POST command on TR-181
     * parameter'Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.'</li>
     * <li>STEP 4.Verification of adding Invalid Ipv6 ping server '2001::2002' by using Webpa POST command on TR-181
     * parameter 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.'</li>
     * <li>STEP 5.Verification of adding Invalid Ipv6 ping server '2002::2003' by using Webpa POST command on TR-181
     * parameter 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.'</li>
     * <li>STEP 6.Verification of adding Invalid Ipv6 ping server '2003::2004' by using Webpa POST command on TR-181
     * parameter 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.'</li>
     * <li>STEP 7.Verify whether 'PING_FAILED:1.2.3.4' message is present in self heallog.txt.0 after the setting the
     * InValid servers.</li>
     * <li>STEP 8.verify whether 'PING_FAILED:2.3.4.5' message is present in self heallog.txt.0 after the setting the
     * InValid servers.</li>
     * <li>STEP 9.verify whether 'PING_FAILED:3.4.5.6' message is present in self heallog.txt.0 after the setting the
     * InValid servers.</li>
     * <li>STEP 10.verify whether 'PING_FAILED:2001::2002' message is present in self heallog.txt.0 after the setting
     * the InValid servers.</li>
     * <li>STEP 11.verify whether 'PING_FAILED:2002::2003' message is present in self heallog.txt.0 after the setting
     * the InValid servers.</li>
     * <li>STEP 12.verify whether 'PING_FAILED:2003::2004' message is present in self heallog.txt.0 after the setting
     * the InValid servers.</li>
     * <li>STEP 13.verify whether 'Ping failure' message is present in rdklogs/logs/selfheallog.txt.0 after the setting
     * the Invalid servers. .</li>
     * <li>STEP 14.:Verify whether 'PING_FAILED:6' message is present in rdklogs/logs/dcmscript.log after getting the
     * ping failure log messages</li>
     * <li>POST-CONDITION 1 : Delete the added ping servers in the Ping Server Table by using WEBPA DELETE command.</li>
     * <li>POST-CONDITION 2 : Begin clear telemetry configuration on the device.
     * </ol>
     *
     * @author Joseph Maduram
     * @refactor Govardhan
     * @param device
     *            {@link Dut}
     */

    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    BroadBandTestGroup.WEBPA }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-NW-CONNECTIVITY-1004")
    public void invalidPingServerConnectivityTestByWebpa(Dut device) {
	// Variable Declaration begins
	// String to store the test case ID
	String testId = "TC-RDKB-NW-CONNECTIVITY-104";
	// stores the test step number
	int stepNumber = 1;
	String step = "S" + stepNumber;
	// String to store the error message
	String errorMessage = null;
	// stores the test status
	boolean status = false;
	// String to store the added table row number
	String tableRowNumber = null;
	// List of String to store the added table row numbers
	List<String> tableRow = new ArrayList<String>();
	// string to store the webpaserver response
	WebPaServerResponse webPaServerResponse = null;
	// Map of string and List for Ping table
	Map<String, List<String>> pingServersTable = new HashMap<String, List<String>>();
	// List of String to store the ping servers
	List<String> pingServers = new ArrayList<String>();
	int preConStepNumber = 0;
	boolean isTelemetryEnabled = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
		BroadBandWebPaConstants.WEBPA_PARAM_FOR_TELEMETRY_2_0_ENABLE, BroadBandTestConstants.TRUE,
		BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS, BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
	// Variable Declaration ends
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-NW-CONNECTIVITY-104");
	LOGGER.info(
		"TEST DESCRIPTION: Verify whether the Invalid ping servers are set and it's failure messages are logged when the ping fails in selfHeal log and verify whether the Ping failures are captured in telemetry dashboard.");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("PRE-CONDITION 1 : Telemetry profile configuration for gateway configuration.");
	LOGGER.info(
		"PRE-CONDITION 2 : Check and Set the Ping Interval to 15 mins 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval'using WebPA.");
	LOGGER.info(
		"Step 1  : Verification of adding Invalid Ipv4 ping server '1.2.3.4' by using Webpa POST command on TR-181 parameter'Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.");
	LOGGER.info(
		"Step 2  : Verification of adding Invalid Ipv4 ping server '2.3.4.5' by using Webpa POST command on TR-181 parameter'Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.");
	LOGGER.info(
		"Step 3  : Verification of adding Invalid Ipv4 ping server '3.4.5.6' by using Webpa POST command on TR-181 parameter'Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.");
	LOGGER.info(
		"Step 4  : Verification of adding Invalid Ipv6 ping server '2001::2002' by using Webpa POST command on TR-181 parameter 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.");
	LOGGER.info(
		"Step 5  : Verification of adding Invalid Ipv6 ping server '2002::2003' by using Webpa POST command on TR-181 parameter 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.");
	LOGGER.info(
		"Step 6  : Verification of adding Invalid Ipv6 ping server '2003::2004' by using Webpa POST command on TR-181 parameter 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable..");
	LOGGER.info(
		"Step 7  : Verify whether 'PING_FAILED:1.2.3.4' message is present in self heallog.txt.0 after the setting the InValid servers.");
	LOGGER.info(
		"Step 8  : Verify whether 'PING_FAILED:2.3.4.5' message is present in self heallog.txt.0 after the setting the InValid servers.");
	LOGGER.info(
		"Step 9  : Verify whether 'PING_FAILED:3.4.5.6' message is present in self heallog.txt.0 after the setting the InValid servers.");
	LOGGER.info(
		"Step 10 : Verify whether 'PING_FAILED:2001::2002' message is present in self heallog.txt.0 after the setting the InValid servers.");
	LOGGER.info(
		"Step 11 : Verify whether 'PING_FAILED:2002::2003' message is present in self heallog.txt.0 after the setting the InValid servers.");
	LOGGER.info(
		"Step 12 : Verify whether 'PING_FAILED:2003::2004' message is present in self heallog.txt.0 after the setting the InValid servers.");
	LOGGER.info(
		"Step 13 : Verify whether 'Ping failure' message is present in rdklogs/logs/selfheallog.txt.0 after the setting the Invalid servers.");
	LOGGER.info(
		"Step 14 : Verify whether 'PING_FAILED:6' message is present in rdklogs/logs/dcmscript.log after getting the ping failure log messages.");
	LOGGER.info(
		"POST-CONDITION 1 : Delete the added ping servers in the Ping Server Table by using WEBPA DELETE command.");
	LOGGER.info("POST-CONDITION 2 :  Begin clear telemetry configuration on the device.");
	LOGGER.info("#######################################################################################");

	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    /**
	     * PRE-CONDITION 1 : TELEMETRY PROFILE CONFIGURATION FOR GATEWAY CONFIGURATION.
	     */
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("PRE-CONDITION 1:DESCRIPTION :  TELEMETRY PROFILE CONFIGURATION FOR GATEWAY CONFIGURATION.");
	    LOGGER.info("PRE-CONDITION 1: ACTION : Telemetry profile Configuration for gateway configuration");
	    LOGGER.info(
		    "PRE-CONDITION 1: EXPECTED : Telemetry profile Configuration for gateway configuration should be successful");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "GATEWAY CONFIGURATION TELEMETRY PROFILE COULD NOT BE CONFIGURED.";
	    // Configure Telemetry Profile
	    // Reboot the Device after updating the DCM Log Server URL.
	    if (!BroadBandTelemetryUtils.configureTelemetryProfileNwConnectivty(tapEnv, device)) {
		errorMessage = "GATEWAY CONFIGURATION TELEMETRY PROFILE COULD NOT BE CONFIGURED.";
		LOGGER.error(errorMessage);
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    }
	    BroadBandCommonUtils.rebootDeviceAsPreCondition(tapEnv, device);
	    // Verify dcmscript log file availability
	    BroadBandCommonUtils.doesFileExistPreCondition(tapEnv, device, BroadBandTestConstants.DCMSCRIPT_LOG_FILE);
	    tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTES);

	    /**
	     * PRE-CONDITION 2 : CHECK AND SET THE PING INTERVAL TO 15 MINS
	     * 'DEVICE.SELFHEAL.CONNECTIVITYTEST.X_RDKCENTRAL-COM_PINGINTERVAL'USING WEBPA.
	     */
	    LOGGER.info("#######################################################################################");
	    LOGGER.info(
		    "PRE-CONDITION 2:DESCRIPTION :  Check and Set the Ping Interval to 15 mins 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval'using WebPA.");
	    LOGGER.info("PRE-CONDITION 2: ACTION : Set the ping Interval to 15 mins using WebPA");
	    LOGGER.info("PRE-CONDITION 2: EXPECTED : ping Interval should be set to 15 mins");
	    LOGGER.info("#######################################################################################");
	    if (!BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.PING_INTERVAL, BroadBandTestConstants.CONSTANT_2,
		    BroadBandTestConstants.CONSTANT_PING_INTERVAL)) {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
			+ "NOT ABLE TO SET THE PING INTERVAL TO 15 MINS - HENCE BLOCKING THE EXECUTION");
	    }
	    LOGGER.info("PRE-CONDITION 2 : ACTUAL: Ping interval is set as 15 mins");

	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

	    /**
	     * STEP 1:VERIFICATION OF ADDING INVALID IPV4 PING SERVER '1.2.3.4' BY USING WEBPA POST COMMAND ON TR-181
	     * PARAMETER'DEVICE.SELFHEAL.CONNECTIVITYTEST.PINGSERVERLIST.IPV4PINGSERVERTABLE.
	     */

	    status = false;
	    String timeStamp = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION :Verification of adding Invalid Ipv4 ping server '1.2.3.4' by using Webpa POST command on TR-181 parameter 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.'");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute webpa command: Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.");
	    LOGGER.info("STEP " + stepNumber
		    + ":  EXPECTED: should be able to set the IPV4 ping server1  as '1.2.3.4'by WebPA");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "Unable to set the IPV4 ping server URI  'Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.1.X_RDKCENTRAL-COM_Ipv4PingServerURI' valid Ipv4 as '1.2.3.4' using WebPA command.";
	    pingServers.add(BroadBandTestConstants.FIRST_IPV4_PINGSERVER_URI);
	    pingServersTable.put(BroadBandWebPaConstants.WEBPA_IPV4_PING_SERVER_URI, pingServers);
	    webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
		    BroadBandWebPaConstants.WEBPA_IPV4_PING_ADD_TABLE, pingServersTable);
	    tableRowNumber = webPaServerResponse.getRow();
	    tableRow.add(tableRowNumber);
	    status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Successfully set the  IPV4 ping server URI  'Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.1.X_RDKCENTRAL-COM_Ipv4PingServerURI' valid Ipv4 as '1.2.3.4' using WebPA command.");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

	    /**
	     * STEP 2:.VERIFICATION OF ADDING INVALID IPV4 PING SERVER '2.3.4.5' BY USING WEBPA POST COMMAND ON TR-181
	     * PARAMETER'DEVICE.SELFHEAL.CONNECTIVITYTEST.PINGSERVERLIST.IPV4PINGSERVERTABLE.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION :Verification of adding Invalid Ipv4 ping server '2.3.4.5' by using Webpa POST command on TR-181 parameter 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.'");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute webpa command: Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED: should be able to set the IPV4 ping server2  as '2.3.4.5'by WebPA");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "Unable to set the  IPV4 ping server URI  'Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.2.X_RDKCENTRAL-COM_Ipv4PingServerURI' Invalid Ipv4 as '2.3.4.5' using WebPA command.";
	    pingServersTable.clear();
	    pingServers.clear();
	    pingServers.add(BroadBandTestConstants.SECOND_IPV4_PINGSERVER_URI);
	    pingServersTable.put(BroadBandWebPaConstants.WEBPA_IPV4_PING_SERVER_URI, pingServers);
	    webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
		    BroadBandWebPaConstants.WEBPA_IPV4_PING_ADD_TABLE, pingServersTable);
	    tableRowNumber = webPaServerResponse.getRow();
	    tableRow.add(tableRowNumber);
	    status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Successfully set the  IPV4 ping server URI  'Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.1.X_RDKCENTRAL-COM_Ipv4PingServerURI' valid Ipv4 as '2.3.4.5' using WebPA command.");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

	    /**
	     * STEP 3:VERIFICATION OF ADDING INVALID IPV4 PING SERVER '3.4.5.6' BY USING WEBPA POST COMMAND ON TR-181
	     * PARAMETER'DEVICE.SELFHEAL.CONNECTIVITYTEST.PINGSERVERLIST.IPV4PINGSERVERTABLE.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION :Verification of adding Invalid Ipv4 ping server '3.4.5.6' by using Webpa POST command on TR-181 parameter 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.'");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute webpa command: Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : should be able to set the IPV4 ping server3  as '3.4.5.6'by WebPA");
	    LOGGER.info("#####################################################################################");
	    errorMessage = "Unable to set the IPV4 ping server URI  'Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.3.X_RDKCENTRAL-COM_Ipv4PingServerURI' Invalid Ipv4 as '3.4.5.6' using WebPA command.";
	    pingServersTable.clear();
	    pingServers.clear();
	    pingServers.add(BroadBandTestConstants.THIRD_IPV4_PINGSERVER_URI);
	    pingServersTable.put(BroadBandWebPaConstants.WEBPA_IPV4_PING_SERVER_URI, pingServers);
	    webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
		    BroadBandWebPaConstants.WEBPA_IPV4_PING_ADD_TABLE, pingServersTable);
	    tableRowNumber = webPaServerResponse.getRow();
	    tableRow.add(tableRowNumber);
	    status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Successfully set the  IPV4 ping server URI  'Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.3.X_RDKCENTRAL-COM_Ipv4PingServerURI' Invalid Ipv4 as '3.4.5.6' using WebPA command.");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

	    /**
	     * STEP 4: VERIFICATION OF ADDING INVALID IPV6 PING SERVER '2001::2002' BY USING WEBPA POST COMMAND ON
	     * TR-181 PARAMETER 'DEVICE.SELFHEAL.CONNECTIVITYTEST.PINGSERVERLIST.IPV6PINGSERVERTABLE.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION :Verification of adding Invalid Ipv6 ping server '2001::2002' by using Webpa POST command on TR-181 parameter 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.'");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute webpa command: Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : Should be able to set the IPV6 ping server1  as '2001::2002'by WebPA");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "Unable to set the IPV6 ping server URI  'Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.1.X_RDKCENTRAL-COM_Ipv6PingServerURI' Invalid Ipv6 as '2001::2002' using WebPA command.";
	    pingServersTable.clear();
	    pingServers.clear();
	    pingServers.add(BroadBandTestConstants.FIRST_IPV6_PINGSERVER_URI);
	    pingServersTable.put(BroadBandWebPaConstants.WEBPA_IPV6_PING_SERVER_URI, pingServers);
	    webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
		    BroadBandWebPaConstants.WEBPA_IPV6_PING_ADD_TABLE, pingServersTable);
	    tableRowNumber = webPaServerResponse.getRow();
	    tableRow.add(tableRowNumber);
	    status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Successfully set the IPV6 ping server URI  'Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.1.X_RDKCENTRAL-COM_Ipv6PingServerURI' Invalid Ipv6 as '2001::2002' using WebPA command.");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

	    /**
	     * STEP 5:VERIFICATION OF ADDING INVALID IPV6 PING SERVER '2002::2003' BY USING WEBPA POST COMMAND ON TR-181
	     * PARAMETER 'DEVICE.SELFHEAL.CONNECTIVITYTEST.PINGSERVERLIST.IPV6PINGSERVERTABLE.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verification of adding Invalid Ipv6 ping server '2002::2003' by using Webpa POST command on TR-181 parameter 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.'");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute webpa command: Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED: Should be able to set the IPV6 ping server2  as '2002::2003'by WebPA");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "Unable to set the IPV6 ping server URI  'Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.2.X_RDKCENTRAL-COM_Ipv6PingServerURI' Invalid Ipv6 as '2002::2003' using WebPA command.";
	    pingServersTable.clear();
	    pingServers.clear();
	    pingServers.add(BroadBandTestConstants.SECOND_IPV6_PINGSERVER_URI);
	    pingServersTable.put(BroadBandWebPaConstants.WEBPA_IPV6_PING_SERVER_URI, pingServers);
	    webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
		    BroadBandWebPaConstants.WEBPA_IPV6_PING_ADD_TABLE, pingServersTable);
	    tableRowNumber = webPaServerResponse.getRow();
	    tableRow.add(tableRowNumber);
	    status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Successfully set the IPV6 ping server URI  'Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.2.X_RDKCENTRAL-COM_Ipv6PingServerURI' Invalid Ipv6 as '2002::2003' using WebPA command.");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

	    /**
	     * STEP 6: VERIFICATION OF ADDING INVALID IPV6 PING SERVER '2003::2004' BY USING WEBPA POST COMMAND ON
	     * TR-181 PARAMETER 'DEVICE.SELFHEAL.CONNECTIVITYTEST.PINGSERVERLIST.IPV6PINGSERVERTABLE.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION: Verification of adding Invalid Ipv6 ping server '2003::2004' by using Webpa POST command on TR-181 parameter 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.'");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute webpa command: Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED: should be able to set the IPV6 ping server3  as '2003::2004'by WebPA");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "Unable to set the IPV6 ping server URI  'Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.3.X_RDKCENTRAL-COM_Ipv6PingServerURI' Invalid Ipv6 as '2003::2004' using WebPA command.";
	    pingServersTable.clear();
	    pingServers.clear();
	    pingServers.add(BroadBandTestConstants.THIRD_IPV6_PINGSERVER_URI);
	    pingServersTable.put(BroadBandWebPaConstants.WEBPA_IPV6_PING_SERVER_URI, pingServers);
	    webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
		    BroadBandWebPaConstants.WEBPA_IPV6_PING_ADD_TABLE, pingServersTable);
	    tableRowNumber = webPaServerResponse.getRow();
	    tableRow.add(tableRowNumber);
	    status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Successfully set the IPV6 ping server URI  'Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.2.X_RDKCENTRAL-COM_Ipv6PingServerURI' Invalid Ipv6 as '2003::2004' using WebPA command.");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

	    /**
	     * STEP 7 :VERIFY WHETHER 'PING_FAILED:1.2.3.4' MESSAGE IS PRESENT IN SELF HEALLOG.TXT.0 AFTER THE SETTING
	     * THE INVALID SERVERS.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION: :Verify whether 'PING_FAILED:1.2.3.4' message  is present in self heallog.txt.0  after the setting the InValid  servers.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute webpa command: PING_FAILED:1.2.3.4\" /rdklogs/logs/SelfHeal.txt.0");
	    LOGGER.info("STEP " + stepNumber
		    + " :EXPECTED - 'PING_FAILED:1.2.3.4' failure message  should  get logged in the next self heal window");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "'PING_FAILED:1.2.3.4' failure message  is not logged in the next self heal window";
	    try {
		status = BroadBandCommonUtils
			.searchLogFiles(tapEnv, device, BroadBandTraceConstants.PING_SERVER_IPV4_FIRST_URI_FAIL,
				BroadBandCommandConstants.FILE_SELFHEAL_LOG,
				BroadBandTestConstants.THIRTY_MINUTES_IN_MILLIS
					+ BroadBandTestConstants.TWENTY_MINUTES_IN_MILLIS,
				BroadBandTestConstants.ONE_MINUTE_IN_MILLIS)
			.contains(BroadBandTraceConstants.PING_SERVER_IPV4_FIRST_URI_FAIL.replace("\"", ""));
	    } catch (TestException exception) {
		errorMessage = exception.getMessage();
		LOGGER.error("error occured while checking the 'PING_FAILED:1.2.3.4' message in self heallog.txt.0  "
			+ errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : PING_FAILED:1.2.3.4' failure message is logged in the next self heal window");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

	    /**
	     * STEP 8 :VERIFY WHETHER 'PING_FAILED:2.3.4.5' MESSAGE IS PRESENT IN SELF HEALLOG.TXT.0 AFTER THE SETTING
	     * THE INVALID SERVERS.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION: verify whether 'PING_FAILED:2.3.4.5' message  is present in self heallog.txt.0  after the setting the InValid  servers.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute command: grep -i \"PING_FAILED:2.3.4.5\" /rdklogs/logs/SelfHeal.txt.0");
	    LOGGER.info("STEP " + stepNumber
		    + " :EXPECTED : PING_FAILED:2.3.4.5' failure message  should  get logged in the next self heal window");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "'PING_FAILED:2.3.4.5' failure message  is not logged in the next self heal window";
	    try {
		status = BroadBandCommonUtils
			.searchLogFiles(tapEnv, device, BroadBandTraceConstants.PING_SERVER_IPV4_SECOND_URI_FAIL,
				BroadBandCommandConstants.FILE_SELFHEAL_LOG, BroadBandTestConstants.FIVE_MINUTES,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)
			.contains(BroadBandTraceConstants.PING_SERVER_IPV4_SECOND_URI_FAIL.replace("\"", ""));
	    } catch (TestException exception) {
		errorMessage = exception.getMessage();
		LOGGER.error("error occured while checking the 'PING_FAILED:2.3.4.5' message in self heallog.txt.0  "
			+ errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : PING_FAILED:2.3.4.5' failure message is logged in the next self heal window");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

	    /**
	     * STEP 9 :VERIFY WHETHER 'PING_FAILED:3.4.5.6' MESSAGE IS PRESENT IN SELF HEALLOG.TXT.0 AFTER THE SETTING
	     * THE INVALID SERVERS.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify whether 'PING_FAILED:3.4.5.6' message  is present in self heallog.txt.0  after the setting the InValid  servers.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute command: grep -i \"PING_FAILED:3.4.5.6\" /rdklogs/logs/SelfHeal.txt.0");
	    LOGGER.info("STEP " + stepNumber
		    + " :EXPECTED : PING_FAILED:3.4.5.6' failure message  should  get logged in the next self heal window");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "'PING_FAILED:3.4.5.6' failure message  is not logged in the next self heal window";
	    try {
		status = BroadBandCommonUtils
			.searchLogFiles(tapEnv, device, BroadBandTraceConstants.PING_SERVER_IPV4_THIRD_URI_FAIL,
				BroadBandCommandConstants.FILE_SELFHEAL_LOG, BroadBandTestConstants.FIVE_MINUTES,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)
			.contains(BroadBandTraceConstants.PING_SERVER_IPV4_THIRD_URI_FAIL.replace("\"", ""));
	    } catch (TestException exception) {
		errorMessage = exception.getMessage();
		LOGGER.error("error occured while checking the 'PING_FAILED:3.4.5.6' message in self heallog.txt.0  "
			+ errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : PING_FAILED:3.4.5.6' failure message is logged in the next self heal window");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

	    /**
	     * STEP 10 : VERIFY WHETHER 'PING_FAILED:2001::2002' MESSAGE IS PRESENT IN SELF HEALLOG.TXT.0 AFTER THE
	     * SETTING THE INVALID SERVERS.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION: Verify whether 'PING_FAILED:2001::2002' message  is present in self heallog.txt.0  after the setting the InValid  servers.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute command: grep -i \"PING_FAILED:2001:2002\" /rdklogs/logs/SelfHeal.txt.0");
	    LOGGER.info("STEP " + stepNumber
		    + " :EXPECTED :PING_FAILED:2001::2002' failure message  should  get logged in the next self heal window");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "'PING_FAILED:2001::2002' failure message  is not logged in the next self heal window";
	    try {
		status = BroadBandCommonUtils
			.searchLogFiles(tapEnv, device, BroadBandTraceConstants.PING_SERVER_IPV6_FIRST_URI_FAIL,
				BroadBandCommandConstants.FILE_SELFHEAL_LOG, BroadBandTestConstants.FIVE_MINUTES,
				BroadBandTestConstants.ONE_MINUTE_IN_MILLIS)
			.contains(BroadBandTraceConstants.PING_SERVER_IPV6_FIRST_URI_FAIL.replace("\"", ""));
	    } catch (TestException exception) {
		errorMessage = exception.getMessage();
		LOGGER.error("error occured while checking the 'PING_FAILED:2001::2002' message in self heallog.txt.0  "
			+ errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : PING_FAILED:2001:2002' failure message is logged in the next self heal window");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

	    /**
	     * STEP 11 :VERIFY WHETHER 'PING_FAILED:2002::2003' MESSAGE IS PRESENT IN SELF HEALLOG.TXT.0 AFTER THE
	     * SETTING THE INVALID SERVERS.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION: verify whether 'PING_FAILED:2002::2003' message  is present in self heallog.txt.0  after the setting the InValid  servers.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute command: grep -i \"PING_FAILED:2002:2003\" /rdklogs/logs/SelfHeal.txt.0");
	    LOGGER.info("STEP " + stepNumber
		    + " :EXPECTED : PING_FAILED:2002::2003' failure message  should  get logged in the next self heal window");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "'PING_FAILED:2002:2003' failure message  is not logged in the next self heal window";
	    try {
		status = BroadBandCommonUtils
			.searchLogFiles(tapEnv, device, BroadBandTraceConstants.PING_SERVER_IPV6_SECOND_URI_FAIL,
				BroadBandCommandConstants.FILE_SELFHEAL_LOG, BroadBandTestConstants.FIVE_MINUTES,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)
			.contains(BroadBandTraceConstants.PING_SERVER_IPV6_SECOND_URI_FAIL.replace("\"", ""));
	    } catch (TestException exception) {
		errorMessage = exception.getMessage();
		LOGGER.error("error occured while checking the 'PING_FAILED:2002::2003' message in self heallog.txt.0  "
			+ errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : PING_FAILED:2002:2003' failure message is logged in the next self heal window");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

	    /**
	     * STEP 12 :VERIFY WHETHER 'PING_FAILED:2003::2004' MESSAGE IS PRESENT IN SELF HEALLOG.TXT.0 AFTER THE
	     * SETTING THE INVALID SERVERS.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : verify whether 'PING_FAILED:2003::2004' message  is present in self heallog.txt.0  after the setting the InValid  servers.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute the command: grep -i \"PING_FAILED:2003:2004\" /rdklogs/logs/SelfHeal.txt.0");
	    LOGGER.info("STEP " + stepNumber
		    + " :EXPECTED :PING_FAILED:2002::2003' failure message  should  get logged in the next self heal window");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "'PING_FAILED:2003:2004' failure message  is not logged in the next self heal window";
	    try {
		status = BroadBandCommonUtils
			.searchLogFiles(tapEnv, device, BroadBandTraceConstants.PING_SERVER_IPV6_THIRD_URI_FAIL,
				BroadBandCommandConstants.FILE_SELFHEAL_LOG, BroadBandTestConstants.FIVE_MINUTES,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)
			.contains(BroadBandTraceConstants.PING_SERVER_IPV6_THIRD_URI_FAIL.replace("\"", ""));
	    } catch (TestException exception) {
		errorMessage = exception.getMessage();
		LOGGER.error("error occured while checking the 'PING_FAILED:2003::2004' message in self heallog.txt.0  "
			+ errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : PING_FAILED:2003:2004' failure message is logged in the next self heal window");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

	    /**
	     * STEP 13 :VERIFY WHETHER 'PING FAILURE' MESSAGE IS PRESENT IN RDKLOGS/LOGS/SELFHEALLOG.TXT.0 AFTER THE
	     * SETTING THE INVALID SERVERS.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION :verify whether 'Ping failure' message  is present in rdklogs/logs/selfheallog.txt.0 after the setting the Invalid  servers.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute the command: grep -i \"Ping to both IPv4 and IPv6 servers are failed\" /rdklogs/logs/SelfHeal.txt.0");
	    LOGGER.info("STEP " + stepNumber
		    + " : EXPECTED: Ping Failure Log  message  should  get logged in the next self heal window");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "'Ping failure' log message is not logged in the next self heal window";
	    try {
		status = BroadBandCommonUtils
			.searchLogFiles(tapEnv, device, BroadBandTraceConstants.PING_SERVER_FAIL_LOG_MESSAGE,
				BroadBandCommandConstants.FILE_SELFHEAL_LOG, BroadBandTestConstants.FIVE_MINUTES,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)
			.contains(BroadBandTraceConstants.PING_SERVER_FAIL_LOG_MESSAGE.replace("\"", ""));
	    } catch (TestException exception) {
		errorMessage = exception.getMessage();
		LOGGER.error("error occured while checking the 'Ping failure' message  in self heallog.txt.0  "
			+ errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Ping failure' log message is successful logged in the next self heal window");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

	    /**
	     * STEP 14 :VERIFY WHETHER 'PING_FAILED:6' MESSAGE IS PRESENT IN RDKLOGS/LOGS/DCMSCRIPT.LOG AFTER GETTING
	     * THE PING FAILURE LOG MESSAGES
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION :Verify whether 'PING_FAILED:6' message  is present in rdklogs/logs/dcmscript.log after getting the ping failure log messages");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute webpa command: . Execute the command  grep -i \"PING_FAILED\":\"6\" /rdklogs/logs/dcmscript.log");
	    LOGGER.info("STEP " + stepNumber
		    + " : EXPECTED - PING_FAILED:6  marker  should  get logged in the rdklogs/logs/dcmscript.log");
	    LOGGER.info("GOING TO SEARCH THE DCMSCRIPT.LOG FOR TELEMETRY MARKER FOR PING FAILURE MESSAGE");
	    LOGGER.info("******************************************************************************");
	    if (!isTelemetryEnabled) {
		JSONObject telemetryPayloadData = BroadBandTelemetryUtils.getPayLoadDataAsJson(tapEnv, device,
			BroadBandTraceConstants.TELEMETRY_MARKER_FOR_PING_SERVER_FAILURE, true);
		LOGGER.info("SEARCHED THE DCMSCRIPT.LOG FOR TELEMETRY MARKER FOR PING FAILURE MESSAGE: "
			+ (null != telemetryPayloadData));
		errorMessage = "Telemetry Marker for Ping Failure is NOT present.";
		if (null != telemetryPayloadData) {
		    String response = BroadBandTelemetryUtils.getPayloadParameterValue(telemetryPayloadData,
			    BroadBandTraceConstants.TELEMETRY_MARKER_FOR_PING_SERVER_FAILURE);
		    LOGGER.info("response is" + response);
		    status = CommonMethods.isNotNull(response) && BroadBandCommonUtils.compareValues("INT_COMPARISON",
			    BroadBandTestConstants.STRING_VALUE_SIX, response);
		    errorMessage = "Telemetry Marker for Ping Failure is  present BUT with inappropriate value";
		}
		if (!status) {
		    errorMessage = "Telemetry Marker for Ping Failure 4 and Ping Failure 2 arenot present";
		    status = BroadBandTelemetryUtils.verifyPingFailedPayloadParamFromDcmScript(tapEnv, device,
			    BroadBandTraceConstants.TELEMETRY_MARKER_FOR_PING_SERVER_FAILURE);
		}
		if (status) {
		    LOGGER.info(
			    "STEP " + stepNumber + " : ACTUAL : TELEMETRY MARKER FOR PING FAILURES IS BEEN PRESENT");
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
	    } else {
		errorMessage = "The count of PING_FAILED is less than 6";
		int count = getTextCount(tapEnv, device,
			BroadBandTraceConstants.TELEMETRY_MARKER_FOR_PING_SERVER_FAILURE,
			BroadBandCommandConstants.FILE_SELFHEAL_LOG, BroadBandTestConstants.FIVE_MINUTES,
			BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		if (count >= BroadBandTestConstants.CONSTANT_6) {
		    status = true;
		}
		if (status) {
		    LOGGER.info("STEP " + stepNumber
			    + " : ACTUAL : For Telemetry 2.0 PING_FAILED count present in selfheal.log is :" + count);
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
	    }
	    LOGGER.info("******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

	} catch (Exception testException) {
	    errorMessage = "Exception occured while checking the invalid ping server connectivity test by webpa"
		    + testException.getMessage();
	    LOGGER.error(errorMessage);
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);
	} finally {
	    int postCondition = 0;
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");

	    /**
	     * POST-CONDITION 1: Delete the added ping servers in the Ping Server Table by using WEBPA DELETE command.
	     */
	    postCondition++;

	    LOGGER.info("#####################################################################################");
	    LOGGER.info("POST-CONDITION " + postCondition
		    + ":  DESCRIPTION :Delete the added ping servers in the Ping Server Table by using WEBPA DELETE command.");
	    LOGGER.info("POST-CONDITION " + postCondition
		    + ": ACTION : Delete the added Ping Servers in the Ping Server Table by using WEBPA DELETE command");
	    LOGGER.info("POST-CONDITION " + postCondition
		    + ": EXPECTED: Should be able to delete the added ping servers in the Ping Server Table using webpa Delete command");
	    LOGGER.info("#####################################################################################");
	    for (int i = 0; i < tableRow.size(); i++) {
		webPaServerResponse = tapEnv.deleteTableRowUsingRestApi(device, tableRow.get(i));
		status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
		if (status) {
		    LOGGER.info("Deleted ping servers table row" + tableRow.get(i));
		}
		if (status) {
		    LOGGER.info("POST-CONDITION " + postCondition + ": ACTUAL : Deleted ping servers table row");
		} else {
		    LOGGER.error("POST-CONDITION " + postCondition + ": ACTUAL : Post condition failed ");
		}
	    }

	    /**
	     * POST CONDITION 2 : Begin clear telemetry configuration on the device.
	     */
	    postCondition++;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("POST-CONDITION " + postCondition
		    + ": DESCRIPTION : Begin clear telemetry configuration on the device.");
	    LOGGER.info("POST-CONDITION " + postCondition + ": ACTION : Clear telemetry configuration on the device");
	    LOGGER.info("POST-CONDITION " + postCondition
		    + ":EXPECTED: Should be able to clear the telemetry configuration on the device");
	    LOGGER.info("#####################################################################################");
	    boolean result = BroadBandTelemetryUtils.clearTelemetryConfiguration(tapEnv, device);
	    if (result) {
		LOGGER.info("POST-CONDITION " + postCondition
			+ ": ACTUAL : End clear telemetry configuration on the device");
	    } else {
		LOGGER.error("POST-CONDITION " + postCondition + ": ACTUAL : Post condition failed");
	    }

	    /**
	     * POST CONDITION 3 : Begin Re-activation operation on the device.
	     */
	    postCondition++;
	    BroadBandPostConditionUtils.executePostConditionToReActivateDevice(device, tapEnv, false, postCondition);

	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-NW-CONNECTIVITY-1004");
    }

    /**
     * method to get number of occurrence of particular text
     * 
     * 
     * @param tapEnv
     *            {@link AutomaticsTapApi}
     * @param device
     *            {@link Dut}
     * @param searchText
     *            String representing the Search Text. It needs to be passed with the required escape character.
     * @param logFile
     *            String representing the log file.
     * @param pollDuration
     *            Long representing the duration for which polling needs to be performed.
     * @param pollInterval
     *            Long representing the polling interval.
     * 
     * @return String representing the search response.
     * @refactor Govardhan
     */
    public static int getTextCount(AutomaticsTapApi tapEnv, Dut device, String searchText, String logFile,
	    long pollDuration, long pollInterval) {
	int count = 0;
	String testArray[] = null;

	LOGGER.debug("STARTING METHOD getTextCount");
	try {
	    StringBuffer sbCommand = new StringBuffer(BroadBandTestConstants.GREP_COMMAND);
	    // In case the search text contains space and not wrapped with double quotes.
	    if (searchText.contains(BroadBandTestConstants.SINGLE_SPACE_CHARACTER)
		    && !searchText.contains(BroadBandTestConstants.DOUBLE_QUOTE)) {
		sbCommand.append(BroadBandTestConstants.DOUBLE_QUOTE);
		sbCommand.append(searchText);
		sbCommand.append(BroadBandTestConstants.DOUBLE_QUOTE);
	    } else {
		sbCommand.append(searchText);
	    }
	    sbCommand.append(BroadBandTestConstants.SINGLE_SPACE_CHARACTER);
	    sbCommand.append(logFile);
	    LOGGER.info("COMMAND TO BE EXECUTED: " + sbCommand.toString());
	    long startTime = System.currentTimeMillis();
	    String searchResponse = null;
	    do {
		tapEnv.waitTill(pollInterval);
		searchResponse = tapEnv.executeCommandUsingSsh(device, sbCommand.toString());
		if (CommonMethods.isNotNull(searchResponse)) {
		    testArray = searchResponse.split(BroadBandTestConstants.STRING_REGEX_MATCH_LINE);
		}
	    } while ((System.currentTimeMillis() - startTime) < pollDuration && CommonMethods.isNull(searchResponse));
	    count = testArray.length;

	    LOGGER.info(" Total Number of occurance of the text to be searched:" + count);
	    LOGGER.debug("ENDING METHOD getTextCount");
	} catch (Exception e) {
	    LOGGER.error(" Exception occured while retrieving the number of occurance of the text " + e.getMessage());
	}
	return count;
    }

    /**
     * Test case is created as part of COVERAGE AUTOMATION . Ping Server to detect network connectivity and Connectivity
     * test using Gateway IP
     * 
     * 
     * TC-RDKB-NW-CONNECTIVITY-1005: Verify whether valid ping servers are set for the ping servers by Webpa and the
     * ping success logs are captured in selfHeallog.txt.0 by snmp
     *
     * <ol>
     * <li>S1)Verify adding the IPV4 ping server table 1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.3.1).</li>
     * <li>S2)Verify setting the IPV4 ping server1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.2.1) 'to valid ipv4
     * ping server.</li>
     * <li>S3)Verify adding the IPV4 ping server table 2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.3.2).</li>
     * <li>S4)Verify setting the IPV4 ping server2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.2.2) to valid ipv4
     * ping server.</li>
     * <li>S5)Verify adding the IPV4 ping server table 3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.3.3).</li>
     * <li>S6)Verify setting the IPV4 ping server3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.2.3) to valid ipv4
     * ping server.</li>
     * <li>S7)Verify adding the IPV6 ping server table 1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.3.1).</li>
     * <li>S8)Verify setting the IPV6 ping server1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.2.1) to valid ipv6
     * ping server</li>
     * <li>S9)Verify adding the IPV6 ping server table 2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.3.2).</li>
     * <li>S10)Verify setting the IPV6 ping server2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.2.2) to valid ipv6
     * ping server.</li>
     * <li>S11)Verify adding the IPV6 ping server table 3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.3.3).</li>
     * <li>S12)Verify setting the IPV6 ping server3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.2.3) to valid ipv6
     * ping server.</li>
     * <li>S13)verify whether 'Connectivity Test is Successfull' message is present in self heallog.txt.0 after the
     * setting the Valid servers by snmp.</li>
     * <li>S14)verify whether 'GW IP Connectivity Test Successfull' mesage is present in rdklogs/logs/selfheallog.txt.0
     * by snmp.</li>
     * </ol>
     *
     * @author Joseph Maduram
     * @refactor anandam
     * 
     * @param device
     *            {@link Dut}
     */

    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    BroadBandTestGroup.WEBPA }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-NW-CONNECTIVITY-1005")

    public void validPingServerConnectivityTestbySnmp(Dut device) {
	// String to store the test case ID
	String testId = "TC-RDKB-NW-CONNECTIVITY-105";
	// String to store the test step number
	String testStepNumber = "s1";
	// String to store the error message
	String errorMessage = null;
	// stores the test status
	boolean status = false;
	String ipv4PingServer = null;
	String ipv6PingServer = null;
	String tableRowNumber = null;
	// Map of string and List for Ping table
	Map<String, List<String>> pingServersTable = new HashMap<String, List<String>>();
	// List of String to store the ping servers
	List<String> pingServers = new ArrayList<String>();
	// List of String to store the added table row numbers
	List<String> tableRowipv4 = new ArrayList<String>();
	List<String> tableRowipv6 = new ArrayList<String>();
	// string to store the webpaserver response
	WebPaServerResponse webPaServerResponse = null;
	String response = null;
	String defaultPingInterval = null;

	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("PRE-CONDITION 1:DESCRIPTION : verify Webpa process is up and running");
	    LOGGER.info(
		    "PRE-CONDITION 1: ACTION : Execute Device.DeviceInfo.SerialNumber and verify Webpa process is up and running'");
	    LOGGER.info("PRE-CONDITION 1: EXPECTED : Webpa process should be up and running");
	    LOGGER.info("#######################################################################################");
	    // If last (third) parameter in verifyWebPaProcessIsUp is set to true, this
	    // command could
	    // potentially run for 8 minutes. As false, it will run no longer than 30
	    // seconds.
	    status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, false);
	    if (status) {
		LOGGER.info("PRE-CONDITION 1: ACTUAL :WEBPA IS UP AND RUNNING - CONTINUING THE EXECUTION");
	    } else {
		LOGGER.error("PRE-CONDITION 1: ACTUAL : Pre condition failed");
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
			+ "WEBPA IS NOT UP AND RUNNING - HENCE BLOCKING THE EXECUTION");
	    }
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("PRE-CONDITION 2:DESCRIPTION : Check and Set the Ping Interval to 15 mins using WebPA.");
	    LOGGER.info(
		    "PRE-CONDITION 2: ACTION : Set the ping Interval to 15 mins using WebPA-'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval'");
	    LOGGER.info("PRE-CONDITION 2: EXPECTED : ping Interval should be set to 15 mins");
	    LOGGER.info("#######################################################################################");
	    defaultPingInterval = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.PING_INTERVAL);
	    if (defaultPingInterval.equalsIgnoreCase(BroadBandTestConstants.CONSTANT_PING_INTERVAL)) {
		status = true;
	    } else {
		status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
			BroadBandWebPaConstants.PING_INTERVAL, BroadBandTestConstants.CONSTANT_2,
			BroadBandTestConstants.CONSTANT_PING_INTERVAL);
	    }
	    if (status) {
		LOGGER.info("PRE-CONDITION 2: ACTUAL : Successfully Set the ping interval to 15 mins");
	    } else {
		LOGGER.error("PRE-CONDITION 2 : ACTUAL : " + errorMessage);
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
			+ "UNABLE TO SET THE PING INTERVAL TO 15 MINS - HENCE BLOCKING THE EXECUTION");
	    }
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-NW-CONNECTIVITY-1005");
	    LOGGER.info(
		    "TEST DESCRIPTION: Verify whether valid IP address are set for the  ping servers by SNMP and the ping success logs are captured in selfHeallog.txt.0");
	    LOGGER.info("#######################################################################################");

	    /**
	     * STEP 1:Verify adding the IPV4 ping server table 1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.3.1).
	     * 
	     */
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    testStepNumber = "s1";
	    status = false;
	    String timeStamp = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 1:Verify adding the IPV4 ping server table 1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.3.1).");
	    LOGGER.info("EXPECTED-Should  be able to add the IPV4 ping server table 1");
	    LOGGER.info("#####################################################################################");

	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_ONE_ADD.getOid(), SnmpDataType.INTEGER,
		    BroadBandTestConstants.CONSTANT_PING_SERVER_ADDTABLE,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_ONE_ADD.getTableIndex());
	    errorMessage = "Unable to Add the IPV4 ping server table1 using SNMP";
	    LOGGER.info("S1 ACTUAL : "
		    + (status ? "Successfully Added the IPV4 ping server table1 using SNMP" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 2:Verify setting the IPV4 ping server1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.2.1) to valid
	     * ipv4 ping server.
	     * 
	     */
	    testStepNumber = "s2";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 2:Verify setting  the IPV4 ping server table 1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.2.1)");
	    LOGGER.info("EXPECTED-Should be able to set the IPV4 ping server table 1 ");
	    LOGGER.info("#####################################################################################");
	    ipv4PingServer = BroadBandNetworkConnectivityUtils.resolvePingServer(device, tapEnv,
		    BroadBandTestConstants.IP_VERSION4);

	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_ONE.getOid(), SnmpDataType.STRING, ipv4PingServer,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_ONE.getTableIndex());
	    errorMessage = "unable to set the IPV4 ping server in table1 using SNMP command";
	    LOGGER.info("S2 ACTUAL : "
		    + (status ? "Successfully set the IPV4 ping server in table1 using SNMP" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 3:Verify adding the IPV4 ping server table 2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.3.2).
	     * 
	     */
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    testStepNumber = "s3";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 3:Verify adding the IPV4 ping server table 2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.3.2).");
	    LOGGER.info("EXPECTED-Should  be able to add the IPV4 ping server table 2");
	    LOGGER.info("#####################################################################################");

	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_TWO_ADD.getOid(), SnmpDataType.INTEGER,
		    BroadBandTestConstants.CONSTANT_PING_SERVER_ADDTABLE,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_TWO_ADD.getTableIndex());
	    errorMessage = "Unable to Add the IPV4 ping server table2 using SNMP";
	    LOGGER.info("S3 ACTUAL : "
		    + (status ? "Successfully Added the IPV4 ping server table2 using SNMP" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 4:Verify setting the IPV4 ping server2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.2.2) to valid
	     * ipv4 ping server.
	     * 
	     */
	    testStepNumber = "s4";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 4:Verify setting  the IPV4 ping server table 2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.2.2)");
	    LOGGER.info("EXPECTED-Should be able to set the IPV4 ping server table 2");
	    LOGGER.info("#####################################################################################");

	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_TWO.getOid(), SnmpDataType.STRING, ipv4PingServer,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_TWO.getTableIndex());
	    errorMessage = "unable to set the IPV4 ping server in table2 using SNMP command ";
	    LOGGER.info("S4 ACTUAL : "
		    + (status ? "Successfully set the IPV4 ping server in table2 using SNMP " : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 5:Verify adding the IPV4 ping server table 3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.3.3).
	     * 
	     */
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    testStepNumber = "s5";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 5:Verify adding the IPV4 ping server table 3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.3.3).");
	    LOGGER.info("EXPECTED-Should  be able to add the IPV4 ping server table 3");
	    LOGGER.info("#####################################################################################");

	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_THREE_ADD.getOid(), SnmpDataType.INTEGER,
		    BroadBandTestConstants.CONSTANT_PING_SERVER_ADDTABLE,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_THREE_ADD.getTableIndex());
	    errorMessage = "Unable to Add the IPV4 ping server table3 using SNMP";
	    LOGGER.info("S5 ACTUAL : "
		    + (status ? "Successfully Added the IPV4 ping server table3 using SNMP" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 6:Verify setting the IPV4 ping server3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.2.3) to valid
	     * ipv4 ping server.
	     * 
	     */
	    testStepNumber = "s6";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 6:Verify setting  the IPV4 ping server table 3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.2.3)");
	    LOGGER.info("EXPECTED-Should be able to set the IPV4 ping server table 3");
	    LOGGER.info("#####################################################################################");

	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_THREE.getOid(), SnmpDataType.STRING, ipv4PingServer,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_THREE.getTableIndex());
	    errorMessage = "unable to set the IPV4 ping server in table3 using SNMP command  ";
	    LOGGER.info("S6 ACTUAL : "
		    + (status ? "Successfully set the IPV4 ping server in table3 using SNMP" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 7:Verify adding the IPV6 ping server table 1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.3.1).
	     * 
	     */
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    testStepNumber = "s7";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 7:Verify adding the IPV6 ping server table 1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.3.1).");
	    LOGGER.info("EXPECTED-Should be able to add the IPV6 ping server table 1");
	    LOGGER.info("#####################################################################################");

	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_ONE_ADD.getOid(), SnmpDataType.INTEGER,
		    BroadBandTestConstants.CONSTANT_PING_SERVER_ADDTABLE,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_ONE_ADD.getTableIndex());
	    errorMessage = "Unable to Add the IPV6 ping server table1 using SNMP";
	    LOGGER.info("S7 ACTUAL : "
		    + (status ? "Successfully Added the IPV6 ping server table1 using SNMP" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 8:Verify setting the IPV6 ping server1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.2.1) to valid
	     * ipv6 ping server.
	     * 
	     */

	    testStepNumber = "s8";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 8:Verify setting  the IPV6 ping server table 1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.2.1)");
	    LOGGER.info("EXPECTED-Should be able to set the IPV6 ping server table 1");
	    LOGGER.info("#####################################################################################");
	    ipv6PingServer = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_WAN_IPV6);

	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_ONE.getOid(), SnmpDataType.STRING, ipv6PingServer,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_ONE.getTableIndex());
	    errorMessage = "unable to set the IPV6 ping server in table1 using SNMP command";
	    LOGGER.info("S8 ACTUAL : "
		    + (status ? "Successfully set the IPV6 ping server in table1 using SNMP" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 9:Verify adding the IPV6 ping server table 2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.3.2).
	     * 
	     */
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    testStepNumber = "s9";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 9:Verify adding the IPV6 ping server table 2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.3.2).");
	    LOGGER.info("EXPECTED-Should  be able to add the IPV6 ping server table 2");
	    LOGGER.info("#####################################################################################");

	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_TWO_ADD.getOid(), SnmpDataType.INTEGER,
		    BroadBandTestConstants.CONSTANT_PING_SERVER_ADDTABLE,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_TWO_ADD.getTableIndex());
	    errorMessage = "Unable to Add the IPV6 ping server table2 using SNMP";
	    LOGGER.info("S9 ACTUAL : "
		    + (status ? "Successfully Added the IPV6 ping server table2 using SNMP" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 10:Verify setting the IPV6 ping server2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.2.2) to
	     * valid ipv6 ping server.
	     * 
	     */
	    testStepNumber = "s10";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 10:Verify setting  the IPV6 ping server table 2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.2.2)");
	    LOGGER.info("EXPECTED-Should be able to set the IPV6 ping server in table 2 ");
	    LOGGER.info("#####################################################################################");

	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_TWO.getOid(), SnmpDataType.STRING, ipv6PingServer,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_TWO.getTableIndex());
	    errorMessage = "unable to set the IPV6 ping server in table2 using SNMP command";
	    LOGGER.info("S10 ACTUAL : "
		    + (status ? "Successfully set the IPV6 ping server in table2 using SNMP" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 11:Verify adding the IPV6 ping server table 3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.3.3).
	     * 
	     */
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    testStepNumber = "s11";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 11:Verify adding the IPV6 ping server table 3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.3.3).");
	    LOGGER.info("EXPECTED-Should  be able to add the IPV6 ping server table 3");
	    LOGGER.info("#####################################################################################");

	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_THREE_ADD.getOid(), SnmpDataType.INTEGER,
		    BroadBandTestConstants.CONSTANT_PING_SERVER_ADDTABLE,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_THREE_ADD.getTableIndex());
	    errorMessage = "Unable to Add the IPV6 ping server table3 using SNMP";
	    LOGGER.info("S11 ACTUAL : "
		    + (status ? "Successfully Added the IPV6 ping server table3 using SNMP" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 12:Verify setting the IPV6 ping server3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.2.3) to
	     * valid ipv6 ping server.
	     * 
	     */
	    testStepNumber = "s12";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 12:Verify setting  the IPV6 ping server table 3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.2.3)");
	    LOGGER.info("EXPECTED-Should be able to set the IPV6 ping server in table 3");
	    LOGGER.info("#####################################################################################");

	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_THREE.getOid(), SnmpDataType.STRING, ipv6PingServer,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_THREE.getTableIndex());
	    errorMessage = "unable to set the IPV6 ping server in table3 using SNMP command";
	    LOGGER.info("S12 ACTUAL : "
		    + (status ? "Successfully set the IPV6 ping server in table3 using SNMP " : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 13 :verify whether "Connectivity Test is Successfull" message is present in self heallog.txt.0 after
	     * the setting the Valid servers.
	     */
	    testStepNumber = "s13";
	    status = false;
	    errorMessage = "Ping server connectivity success message is not logged in the next self heal window";
	    String timeFromFirstLog = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 13:DESCRIPTION:verify whether 'Connectivity Test is Successfull' message  is present in self heallog.txt.0  after the setting the Valid ping servers. ");
	    LOGGER.info(
		    "STEP 13:ACTION: validate whether Connectivity Test is Successfull message  is present in self heallog.txt.0 after setting the required servers");
	    LOGGER.info(
		    "STEP 13:EXPECTED - Valid Ping server connectivity Test success message  should  get logged in the next self heal window");
	    LOGGER.info("**********************************************************************************");
	    try {
		response = BroadBandCommonUtils.searchLogByPolling(tapEnv, device,
			BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.GREP_COMMAND,
				BroadBandTraceConstants.PING_SERVER_SUCCESS_LOG_MESSAGE,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
				BroadBandCommandConstants.FILE_SELFHEAL_LOG, BroadBandTestConstants.SYMBOL_PIPE,
				BroadBandTestConstants.CMD_TAIL_1),
			BroadBandTestConstants.SIXTY_MINUTES_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		status = CommonMethods.isNotNull(response) && CommonUtils.patternSearchFromTargetString(response,
			BroadBandTraceConstants.PING_SERVER_SUCCESS_LOG_MESSAGE.replace(
				BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandTestConstants.EMPTY_STRING));
		if (status) {
		    timeFromFirstLog = CommonMethods.patternFinder(response,
			    BroadBandTestConstants.PATTERN_FINDER_TO_GREP_TIMESTAMP);
		    LOGGER.info("Timestamp from the log is-" + timeFromFirstLog);
		}
	    } catch (Exception e) {
		LOGGER.error(
			"Exception occurred while verifying connectivity message in selfheallog: " + e.getMessage());
	    }
	    if (status) {

		LOGGER.info(
			"STEP 13: ACTUAL : Ping server connectivity success message is logged in the self heal window");
	    } else {
		LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 14 :verify whether "GW IP Connectivity Test Successfull" message is present in
	     * rdklogs/logs/selfheallog.txt.0 .
	     */
	    testStepNumber = "s14";
	    errorMessage = "Gateway connectivity success message is not logged in the self heal window";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 14:DESCRIPTION: verify whether 'GW IP Connectivity Test Successfull' message  is present in rdklogs/logs/selfheallog.txt.0 . ");
	    LOGGER.info(
		    "STEP 14:ACTION:validate whether GW IP Connectivity Test Successfull message  is present in rdklogs/logs/selfheallog.txt.0");
	    LOGGER.info(
		    "STEP 14:EXPECTED - Gateway connectivity success message  should  get logged in the  self heal window");
	    LOGGER.info("**********************************************************************************");
	    response = BroadBandCommonUtils.searchLogByPolling(tapEnv, device,
		    BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.GREP_COMMAND,
			    BroadBandTraceConstants.PING_SERVER_GATEWAY_IP_SUCCESS_LOG_MESSAGE,
			    BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandCommandConstants.FILE_SELFHEAL_LOG,
			    BroadBandTestConstants.SYMBOL_PIPE, BroadBandTestConstants.CMD_TAIL_1),
		    BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
	    status = CommonMethods.isNotNull(response) && CommonUtils.patternSearchFromTargetString(response,
		    BroadBandTraceConstants.PING_SERVER_GATEWAY_IP_SUCCESS_LOG_MESSAGE
			    .replace(BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandTestConstants.EMPTY_STRING));
	    if (status) {
		LOGGER.info("STEP 14: ACTUAL : Gateway connectivity is successful in the self heal window");
	    } else {
		LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 15 :verify whether 'Connectivity Test is Successfull' message is present in self heallog.txt.0 in
	     * the next cycle for set ping interval time of 15 mins
	     */
	    testStepNumber = "s15";
	    errorMessage = "Ping server connectivity success message is not logged in the next cycle self heal window for set ping interval time of 15 mins";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 15:DESCRIPTION: verify whether 'Connectivity Test is Successfull' message  is present in self heallog.txt.0  in the next cycle for set ping interval time of 15 mins.");
	    LOGGER.info(
		    "STEP 15:ACTION: validate whether Connectivity Test is Successfull message  is present in self heallog.txt.0 in the next cycle for set ping interval time of 15 mins");
	    LOGGER.info(
		    "STEP 15:EXPECTED - Valid Ping server connectivity Test success message  should  get logged in the next cycle self heal window");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandCommonUtils.pingTestForSetIntervalTime(device, tapEnv, timeFromFirstLog);
	    if (status) {
		LOGGER.info(
			"STEP 15: ACTUAL : Ping server connectivity success message is logged in the next cycle self heal window for set ping interval time of 15 mins");
	    } else {
		LOGGER.error("STEP 15: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	}

	catch (Exception testException) {
	    errorMessage = "Exception occured while checking connectivity for valid ping servers "
		    + testException.getMessage();
	    LOGGER.error(errorMessage);
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
	}

	finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "POST-CONDITION 1: DESCRIPTION : Clear/Reset the  table row of ipv4 by adding a single table row");
	    LOGGER.info("POST-CONDITION 1: ACTION : Verify adding a table row as a ipv4 single server");
	    LOGGER.info("POST-CONDITION 1:EXPECTED: Should be able to add the add  table as a ipv4 single server");
	    LOGGER.info("#####################################################################################");
	    pingServersTable.clear();
	    pingServers.clear();
	    tableRowipv4.clear();
	    tableRowipv6.clear();
	    pingServers.add(BroadBandTestConstants.THIRD_IPV4_PINGSERVER_URI);
	    pingServersTable.put(BroadBandWebPaConstants.WEBPA_IPV4_PING_SERVER_URI, pingServers);
	    webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
		    BroadBandWebPaConstants.WEBPA_IPV4_PING_ADD_TABLE, pingServersTable);
	    tableRowNumber = webPaServerResponse.getRow();
	    tableRowipv4.add(tableRowNumber);
	    status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
	    LOGGER.info("Added a single table row for Ipv4 ping server-" + status);

	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "POST-CONDITION 2: DESCRIPTION : Clear/Reset the  table row of ipv6 by adding a single table row");
	    LOGGER.info("POST-CONDITION 2 : ACTION : Verify adding a table row as a ipv4 single server");
	    LOGGER.info("POST-CONDITION 2:EXPECTED: Should be able to add the add  table as a ipv4 single server");
	    LOGGER.info("#####################################################################################");
	    pingServersTable.clear();
	    pingServers.clear();
	    pingServers.add(BroadBandTestConstants.THIRD_IPV6_PINGSERVER_URI);
	    pingServersTable.put(BroadBandWebPaConstants.WEBPA_IPV6_PING_SERVER_URI, pingServers);
	    webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
		    BroadBandWebPaConstants.WEBPA_IPV6_PING_ADD_TABLE, pingServersTable);
	    tableRowNumber = webPaServerResponse.getRow();
	    tableRowipv6.add(tableRowNumber);
	    status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
	    LOGGER.info("Added a single table row for Ipv6 ping server-" + status);

	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "POST-CONDITION 3: DESCRIPTION :Delete the added ipv4 ping servers in the Ping Server Table by using WEBPA DELETE command.");
	    LOGGER.info(
		    "POST-CONDITION 3: ACTION : Delete the added Ping Servers in the ipv4 Ping Server Table by using WEBPA DELETE command");
	    LOGGER.info(
		    "POST-CONDITION 3:EXPECTED: Should be able to delete the added ipv4 ping servers in the Ping Server Table using webpa Delete command");
	    LOGGER.info("#####################################################################################");
	    String num = CommonMethods.patternFinder(tableRowipv4.get(0),
		    BroadBandTestConstants.PATTERN_MATCHER_TO_GET_TABLE_ROWS);
	    for (int count = 1; count <= Integer.parseInt(num); count++) {
		webPaServerResponse = tapEnv.deleteTableRowUsingRestApi(device,
			"Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable." + count + ".");
		status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
		LOGGER.info("Deleted ping servers table row="
			+ "Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable." + count + "." + "-"
			+ status);
	    }

	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "POST-CONDITION 4: DESCRIPTION :Delete the added ipv6 ping servers in the Ping Server Table by using WEBPA DELETE command.");
	    LOGGER.info(
		    "POST-CONDITION 4: ACTION : Delete the added Ping Servers in the ipv6 Ping Server Table by using WEBPA DELETE command");
	    LOGGER.info(
		    "POST-CONDITION 4:EXPECTED: Should be able to delete the added ipv6 ping servers in the Ping Server Table using webpa Delete command");
	    LOGGER.info("#####################################################################################");
	    num = CommonMethods.patternFinder(tableRowipv6.get(0),
		    BroadBandTestConstants.PATTERN_MATCHER_TO_GET_TABLE_ROWS);
	    for (int count = 1; count <= Integer.parseInt(num); count++) {
		webPaServerResponse = tapEnv.deleteTableRowUsingRestApi(device,
			"Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable." + count + ".");
		status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
		LOGGER.info("Deleted ping servers table row for ipv6="
			+ "Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.-" + count + ".");

	    }
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("POST-CONDITION 5: DESCRIPTION : set the ping interval to default value");
	    LOGGER.info(
		    "POST-CONDITION 5: ACTION : Set the ping Interval to default time interval using WebPA-'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval'");
	    LOGGER.info("POST-CONDITION 5: EXPECTED : Successfully set the default ping interval to default value");
	    LOGGER.info("#####################################################################################");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.PING_INTERVAL, BroadBandTestConstants.CONSTANT_2, defaultPingInterval);
	    if (status) {
		LOGGER.info("POST-CONDITION 5: ACTUAL: Successfully set the default ping interval");
	    } else {
		LOGGER.info("POST-CONDITION 5: ACTUAL: Failed to set the default ping interval");
	    }
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
    }

    /**
     * 
     * 
     * Test Case : TC-RDKB-NW-CONNECTIVITY-1006: Verify whether Invalid Ping servers are set for the ping servers by
     * Webpa and the ping failure logs are captured in selfHeallog.txt.0
     *
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>PRE-CONDITION 1 : Telemetry profile configuration for gateway configuration.</li>
     * <li>PRE-CONDITION 2 : Check and Set the Ping Interval to 15 mins
     * 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval'using WebPA.</li>
     * <li>STEP 1.Verify adding the IPV4 ping server table 1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.3.1).</li>
     * <li>STEP 2.Verify setting the IPV4 ping server1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.2.1) as
     * '1.2.3.4'.</li>
     * <li>STEP 3.Verify adding the IPV4 ping server table 2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.3.2).</li>
     * <li>STEP 4.Verify setting the IPV4 ping server2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.2.2) as
     * '2.3.4.5'.</li>
     * <li>STEP 5.Verify adding the IPV4 ping server table 3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.3.3).</li>
     * <li>STEP 6.Verify setting the IPV4 ping server3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.2.3) as
     * '3.4.5.6'.</li>
     * <li>STEP 7.Verify adding the IPV6 ping server table 1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.3.1).</li>
     * <li>STEP 8.Verify setting the IPV6 ping server1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.2.1) as
     * '2001::2002'.</li>
     * <li>STEP 9.Verify adding the IPV6 ping server table 2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.3.2).</li>
     * <li>STEP 10.)Verify setting the IPV6 ping server2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.2.2)
     * as'2002::2003'.</li>
     * <li>STEP 11.Verify adding the IPV6 ping server table 3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.3.3).</li>
     * <li>STEP 12.Verify setting the IPV6 ping server3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.2.3)
     * as'2003::2004'.</li>
     * <li>STEP 13.verify whether "PING_FAILED:1.2.3.4" message is present in self heallog.txt.0 after the setting of
     * Invalid ping servers.</li>
     * <li>STEP 14.verify whether "PING_FAILED:2.3.4.5" message is present in self heallog.txt.0 after the setting
     * ofInvalid ping servers.</li>
     * <li>STEP 15.verify whether "PING_FAILED:3.4.5.6" message is present in self heallog.txt.0 after the setting
     * ofInvalid ping servers.</li>
     * <li>STEP 16.verify whether "PING_FAILED:2001:2002" message is present in self heallog.txt.0 after the setting
     * ofInvalid ping servers.</li>
     * <li>STEP 17.verify whether "PING_FAILED:2002:2003" message is present in self heallog.txt.0 after the setting
     * ofInvalid ping servers.</li>
     * <li>STEP 18.verify whether "PING_FAILED:2003:2004" message is present in self heallog.txt.0 after the setting
     * ofInvalid ping servers.</li>
     * <li>STEP 19.verify whether "Ping failure" message is present in rdklogs/logs/selfheallog.txt.0 after the setting
     * theInvalid servers.</li>
     * <li>STEP 20.:Verify whether 'PING_FAILED:6' message is present in rdklogs/logs/dcmscript.log after getting the
     * pingfailure log messages</li>
     * <li>POST-CONDITION 1 : Clear/Reset the table row of ipv4 by adding a single table row.</li>
     * <li>POST-CONDITION 2 : Clear/Reset the table row of ipv6 by adding a single table row.</li>
     * <li>POST-CONDITION 3 : Delete the added ipv4 ping servers in the Ping Server Table by using WEBPA DELETE
     * command.</li>
     * <li>POST-CONDITION 4 : Delete the added ipv6 ping servers in the Ping Server Table by using WEBPA DELETE
     * command.</li>
     * <li>POST-CONDITION 5 : Begin clear telemetry configuration on the device.</li>
     * <li>POST-CONDITION 6 : Begin re-activation operation on the device.</li>
     * </ol>
     *
     * @author Joseph Maduram
     * 
     * @param device
     *            {@link Dut}
     */

    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    BroadBandTestGroup.WEBPA }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-NW-CONNECTIVITY-1006")

    public void invalidPingServerConnectivityTestbySnmp(Dut device) {
	// String to store the test case ID
	String testId = "TC-RDKB-NW-CONNECTIVITY-106";
	// stores the test step number
	int stepNumber = 1;
	String step = "S" + stepNumber;
	// String to store the error message
	String errorMessage = null;
	// stores the test status
	boolean status = false;
	// string to store the webpaserver response
	WebPaServerResponse webPaServerResponse = null;
	String tableRowNumber = null;
	// Map of string and List for Ping table
	Map<String, List<String>> pingServersTable = new HashMap<String, List<String>>();
	// List of String to store the ping servers
	List<String> pingServers = new ArrayList<String>();
	// List of String to store the added table row numbers
	List<String> tableRowipv4 = new ArrayList<String>();
	List<String> tableRowipv6 = new ArrayList<String>();
	boolean isTelemetryEnabled = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
		BroadBandWebPaConstants.WEBPA_PARAM_FOR_TELEMETRY_2_0_ENABLE, BroadBandTestConstants.TRUE,
		BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS, BroadBandTestConstants.TEN_SECOND_IN_MILLIS);

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-NW-CONNECTIVITY-106");
	LOGGER.info(
		"TEST DESCRIPTION: Verify whether the Invalid ping servers are set and it's failure messages are logged when the ping fails in selfHeal log and verify whether the Ping failures are captured in telemetry dashboard.");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("PRE-CONDITION 1 : Telemetry profile configuration for gateway configuration.");
	LOGGER.info(
		"PRE-CONDITION 2 : Check and Set the Ping Interval to 15 mins 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval'using WebPA.");
	LOGGER.info(
		"Step 1  : Verify adding the IPV4 ping server table 1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.3.1).");
	LOGGER.info(
		"Step 2  : Verify setting the IPV4 ping server1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.2.1) as '1.2.3.4'.");
	LOGGER.info(
		"Step 3  : Verify adding the IPV4 ping server table 2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.3.2).");
	LOGGER.info(
		"Step 4  : Verify setting the IPV4 ping server2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.2.2) as '2.3.4.5'.");
	LOGGER.info(
		"Step 5  : Verify adding the IPV4 ping server table 3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.3.3).");
	LOGGER.info(
		"Step 6  : Verify setting the IPV4 ping server3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.2.3) as '3.4.5.6'.");
	LOGGER.info(
		"Step 7  : Verify adding the IPV6 ping server table 1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.3.1).");
	LOGGER.info(
		"Step 8  : Verify setting the IPV6 ping server1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.2.1) as '2001::2002'.");
	LOGGER.info(
		"Step 9  : Verify adding the IPV6 ping server table 2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.3.2).");
	LOGGER.info(
		"Step 10 : Verify setting the IPV6 ping server2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.2.2) as'2002::2003'.");
	LOGGER.info(
		"Step 11 :Verify adding the IPV6 ping server table 3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.3.3).");
	LOGGER.info(
		"Step 12 : Verify setting the IPV6 ping server3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.2.3) as'2003::2004'.");
	LOGGER.info(
		"Step 13 : verify whether PING_FAILED:1.2.3.4 message is present in self heallog.txt.0 after the setting of Invalid ping servers.");
	LOGGER.info(
		"Step 14 : verify whether PING_FAILED:2.3.4.5 message is present in self heallog.txt.0 after the setting ofInvalid ping servers.");
	LOGGER.info(
		"Step 15 : verify whether PING_FAILED:3.4.5.6 message is present in self heallog.txt.0 after the setting ofInvalid ping servers.");
	LOGGER.info(
		"Step 16 : verify whether PING_FAILED:2001:2002 message is present in self heallog.txt.0 after the setting ofInvalid ping servers.");
	LOGGER.info(
		"Step 17 : verify whether PING_FAILED:2002:2003 message is present in self heallog.txt.0 after the setting ofInvalid ping servers.");
	LOGGER.info(
		"Step 18 : verify whether PING_FAILED:2003:2004 message is present in self heallog.txt.0 after the setting ofInvalid ping servers.");
	LOGGER.info(
		"Step 19 :verify whether Ping failure message is present in rdklogs/logs/selfheallog.txt.0 after the setting theInvalid servers.");
	LOGGER.info(
		"Step 20 : Verify whether 'PING_FAILED:6' message is present in rdklogs/logs/dcmscript.log after getting the pingfailure log messages.");
	LOGGER.info("POST-CONDITION 1 : Clear/Reset the  table row of ipv4 by adding a single table row");
	LOGGER.info("POST-CONDITION 2 :  Clear/Reset the  table row of ipv6 by adding a single table row");
	LOGGER.info(
		"POST-CONDITION 3 :  Delete the added ipv4 ping servers in the Ping Server Table by using WEBPA DELETE command.");
	LOGGER.info(
		"POST-CONDITION 4 :  Delete the added ipv6 ping servers in the Ping Server Table by using WEBPA DELETE command.");
	LOGGER.info("POST-CONDITION 5 :  Begin clear telemetry configuration on the device.");
	LOGGER.info("POST-CONDITION 6 :	 Begin re-activation operation on the device.");
	LOGGER.info("#######################################################################################");
	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    /**
	     * PRE-CONDITION 1 : TELEMETRY PROFILE CONFIGURATION FOR GATEWAY CONFIGURATION.
	     */
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("PRE-CONDITION 1:DESCRIPTION :  TELEMETRY PROFILE CONFIGURATION FOR GATEWAY CONFIGURATION.");
	    LOGGER.info("PRE-CONDITION 1: ACTION : Telemetry profile Configuration for gateway configuration");
	    LOGGER.info(
		    "PRE-CONDITION 1: EXPECTED : Telemetry profile Configuration for gateway configuration should be successful");
	    LOGGER.info("#######################################################################################");
	    // Configure Telemetry Profile
	    // Reboot the Device after updating the DCM Log Server URL.
	    if (!BroadBandTelemetryUtils.configureTelemetryProfileNwConnectivty(tapEnv, device)) {
		errorMessage = "GATEWAY CONFIGURATION TELEMETRY PROFILE COULD NOT BE CONFIGURED.";
		LOGGER.error(errorMessage);
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    }
	    BroadBandCommonUtils.rebootDeviceAsPreCondition(tapEnv, device);
	    // Verify dcmscript log file availability
	    BroadBandCommonUtils.doesFileExistPreCondition(tapEnv, device, BroadBandTestConstants.DCMSCRIPT_LOG_FILE);
	    tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTES);

	    /**
	     * PRE-CONDITION 2 : CHECK AND SET THE PING INTERVAL TO 15 MINS
	     * 'DEVICE.SELFHEAL.CONNECTIVITYTEST.X_RDKCENTRAL-COM_PINGINTERVAL'USING WEBPA.
	     */
	    LOGGER.info("#######################################################################################");
	    LOGGER.info(
		    "PRE-CONDITION 2:DESCRIPTION :  Check and Set the Ping Interval to 15 mins 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval'using WebPA.");
	    LOGGER.info("PRE-CONDITION 2: ACTION : Set the ping Interval to 15 mins using WebPA");
	    LOGGER.info("PRE-CONDITION 2: EXPECTED : ping Interval should be set to 15 mins");
	    LOGGER.info("#######################################################################################");
	    if (!BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.PING_INTERVAL, BroadBandTestConstants.CONSTANT_2,
		    BroadBandTestConstants.CONSTANT_PING_INTERVAL)) {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
			+ "NOT ABLE TO SET THE PING INTERVAL TO 15 MINS - HENCE BLOCKING THE EXECUTION");
	    }
	    LOGGER.info("PRE-CONDITION 2 : ACTUAL: Ping interval is set as 15 mins");

	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

	    /**
	     * STEP 1:Verify adding the IPV4 ping server table 1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.3.1).
	     * 
	     */

	    tapEnv.waitTill(BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS);
	    status = false;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION :Verify adding the IPV4 ping server table 1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.3.1).");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute snmp command : snmpset -v 2c -c string1 10.254.35.35 .1.3.6.1.4.1.17270.44.1.1.6.1.1.3.1 i 5");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Should  be able to add the IPV4 ping server table 1");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "Unable to Add the IPV4 ping server table1 using SNMP";
	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_ONE_ADD.getOid(), SnmpDataType.INTEGER,
		    BroadBandTestConstants.CONSTANT_PING_SERVER_ADDTABLE,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_ONE_ADD.getTableIndex());
	    if (status) {
		LOGGER.info(
			"STEP " + stepNumber + " : ACTUAL : Successfully Added the IPV4 ping server table1 using SNMP");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

	    /**
	     * STEP 2:Verify setting the IPV4 ping server 1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.2.1) as
	     * '1.2.3.4'.
	     * 
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION :Verify setting  the IPV4 ping server 1  using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.2.1) as '1.2.3.4'");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute snmp command : snmpset -v 2c -c string1 10.254.35.35 .1.3.6.1.4.1.17270.44.1.1.6.1.1.2.1 s 1.2.3.4");
	    LOGGER.info(
		    "STEP " + stepNumber + ":  EXPECTED: Should be able to set the IPV4 ping server 1 as '1.2.3.4'");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "Unable to set the IPV4 ping server table1 using SNMP as '1.2.3.4' ";
	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_ONE.getOid(), SnmpDataType.STRING,
		    BroadBandTestConstants.FIRST_IPV4_PINGSERVER_URI,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_ONE.getTableIndex());
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Successfully set the IPV4 ping server table1 using SNMP as '1.2.3.4' ");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

	    /**
	     * STEP 3:Verify adding the IPV4 ping server table 2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.3.2).
	     * 
	     */
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify adding the IPV4 ping server table 2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.3.2).");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute snmp command : snmpset -v 2c -c string1 10.254.35.35 .1.3.6.1.4.1.17270.44.1.1.6.1.1.3.2 i 5");
	    LOGGER.info("STEP " + stepNumber + ":  EXPECTED: Should  be able to add the IPV4 ping server table 2");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "Unable to Add the IPV4 ping server table2 using SNMP";
	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_TWO_ADD.getOid(), SnmpDataType.INTEGER,
		    BroadBandTestConstants.CONSTANT_PING_SERVER_ADDTABLE,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_TWO_ADD.getTableIndex());
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Successfully Added the IPV4 ping server table2 using SNMP ");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

	    /**
	     * STEP 4:Verify setting the IPV4 ping server 2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.2.2) as
	     * '2.3.4.5'.
	     * 
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION :Verify setting  the IPV4 ping server 2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.2.2) as '2.3.4.5'");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute snmp command : snmpset -v 2c -c string1 10.254.35.35 .1.3.6.1.4.1.17270.44.1.1.6.1.1.2.2 s 2.3.4.5");
	    LOGGER.info(
		    "STEP " + stepNumber + ":  EXPECTED : Should be able to set the IPV4 ping server 2 as '2.3.4.5'");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "Unable to set the IPV4 ping server table2 using SNMP as '2.3.4.5' ";
	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_TWO.getOid(), SnmpDataType.STRING,
		    BroadBandTestConstants.SECOND_IPV4_PINGSERVER_URI,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_TWO.getTableIndex());
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Successfully set the IPV4 ping server table2 using SNMP as '2.3.4.5'");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

	    /**
	     * STEP 5:Verify adding the IPV4 ping server table 3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.3.3).
	     * 
	     */
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify adding the IPV4 ping server table 3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.3.3).");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute snmp command : snmpset -v 2c -c string1 10.254.35.35 .1.3.6.1.4.1.17270.44.1.1.6.1.1.3.3 i 5");
	    LOGGER.info("STEP " + stepNumber + ":  EXPECTED : Should  be able to add the IPV4 ping server table 3");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Unable to Add the IPV4 ping server table3 using SNMP";
	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_THREE_ADD.getOid(), SnmpDataType.INTEGER,
		    BroadBandTestConstants.CONSTANT_PING_SERVER_ADDTABLE,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_THREE_ADD.getTableIndex());
	    if (status) {
		LOGGER.info(
			"STEP " + stepNumber + " : ACTUAL : Successfully Added the IPV4 ping server table3 using SNMP");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

	    /**
	     * STEP 6:Verify setting the IPV4 ping server 3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.2.3) as
	     * '3.4.5.6'.
	     * 
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION :Verify setting  the IPV4 ping server 3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.2.3) as '3.4.5.6'");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute snmp command : snmpset -v 2c -c string1 10.254.35.35 .1.3.6.1.4.1.17270.44.1.1.6.1.1.2.3 s 3.4.5.6");
	    LOGGER.info(
		    "STEP " + stepNumber + ":  EXPECTED :Should be able to set the IPV4 ping server 3 as '3.4.5.6'");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "Unable to set the IPV4 ping server table3 using SNMP as '3.4.5.6' ";
	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_THREE.getOid(), SnmpDataType.STRING,
		    BroadBandTestConstants.THIRD_IPV4_PINGSERVER_URI,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_THREE.getTableIndex());
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Successfully set the IPV4 ping server table3 using SNMP as '3.4.5.6' ");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

	    /**
	     * STEP 7:Verify adding the IPV6 ping server table 1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.3.1).
	     * 
	     */
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION :Verify adding the IPV6 ping server table 1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.3.1).");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute snmp command : snmpset -v 2c -c string1 10.254.35.35 .1.3.6.1.4.1.17270.44.1.1.6.1.1.3.1 i 5");
	    LOGGER.info("STEP " + stepNumber + ":  EXPECTED : Should be able to add the IPV6 ping server table 1");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "Unable to add the the IPV6 ping server table1 using SNMP";
	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_ONE_ADD.getOid(), SnmpDataType.INTEGER,
		    BroadBandTestConstants.CONSTANT_PING_SERVER_ADDTABLE,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_ONE_ADD.getTableIndex());
	    if (status) {
		LOGGER.info(
			"STEP " + stepNumber + " : ACTUAL : Successfully Added the IPV6 ping server table1 using SNMP");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

	    /**
	     * STEP 8:Verify setting the IPV6 ping server1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.2.1) as
	     * '2001::2002'.
	     * 
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify setting  the IPV6 ping server 1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.2.1) as '2001::2002'.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute snmp command : snmpset -v 2c -c string1 10.254.35.35 .1.3.6.1.4.1.17270.44.1.1.6.2.1.2.1 s 2001::2002");
	    LOGGER.info("STEP " + stepNumber
		    + ":  EXPECTED : Should be able to set the IPV6 ping server 1 as '2001::2002'");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "Unable to set the IPV6 ping server table1 using SNMP as '2001::2002' ";
	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_ONE.getOid(), SnmpDataType.STRING,
		    BroadBandTestConstants.FIRST_IPV6_PINGSERVER_URI,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_ONE.getTableIndex());

	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Successfully set the IPV6 ping server table1 using SNMP as '2001::2002' ");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

	    /**
	     * STEP 9:Verify adding the IPV6 ping server table 2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.3.2).
	     * 
	     */
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify adding the IPV6 ping server table 2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.3.2).");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute snmp command : snmpset -v 2c -c string1 10.254.35.35 .1.3.6.1.4.1.17270.44.1.1.6.2.1.3.2 i 5");
	    LOGGER.info("STEP " + stepNumber + ":  EXPECTED : Should  be able to add the IPV6 ping server table 2");
	    LOGGER.info("#####################################################################################");
	    errorMessage = "Unable to add the the IPV6 ping server table2 using SNMP";
	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_TWO_ADD.getOid(), SnmpDataType.INTEGER,
		    BroadBandTestConstants.CONSTANT_PING_SERVER_ADDTABLE,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_TWO_ADD.getTableIndex());
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Successfully Added the IPV6 ping server table2 using SNMP ");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

	    /**
	     * STEP 10:Verify setting the IPV6 ping server2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.2.2) as
	     * '2002::2003'
	     * 
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify setting  the IPV6 ping server 2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.2.2) as '2002::2003'");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute snmp command : snmpset -v 2c -c string1 10.254.35.35 .1.3.6.1.4.1.17270.44.1.1.6.2.1.2.2 s 2002::2003");
	    LOGGER.info("STEP " + stepNumber
		    + ":  EXPECTED :  Should be able to set the IPV6 ping server 2 as '2002::2003'");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "Unable to set the IPV6 ping server table2 using SNMP as '2002::2003' ";
	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_TWO.getOid(), SnmpDataType.STRING,
		    BroadBandTestConstants.SECOND_IPV6_PINGSERVER_URI,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_TWO.getTableIndex());
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Successfully set the IPV6 ping server table2 using SNMP as '2002::2003'  ");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

	    /**
	     * STEP 11:Verify adding the IPV6 ping server table 3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.3.3).
	     * 
	     * 
	     */
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify adding the IPV6 ping server table 3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.3.3).");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute the snmp command: snmpset -v 2c -c string1 10.254.35.35 .1.3.6.1.4.1.17270.44.1.1.6.2.1.3.3 i 5");
	    LOGGER.info("STEP " + stepNumber + ":  EXPECTED : Should  be able to add the IPV6 ping server table 3");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "Unable to Add the IPV6 ping server table3 using SNMP";
	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_THREE_ADD.getOid(), SnmpDataType.INTEGER,
		    BroadBandTestConstants.CONSTANT_PING_SERVER_ADDTABLE,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_THREE_ADD.getTableIndex());
	    if (status) {
		LOGGER.info(
			"STEP " + stepNumber + " : ACTUAL : Successfully Added the IPV6 ping server table3 using SNMP");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

	    /**
	     * STEP 12:Verify setting the IPV6 ping server3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.2.3) as
	     * '2003::2004'.
	     * 
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify setting  the IPV6 ping server 3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.2.3) as '2003::2004'.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute the snmp command: snmpset -v 2c -c string1 10.254.35.35 .1.3.6.1.4.1.17270.44.1.1.6.2.1.2.3 s 2003::2004");
	    LOGGER.info("STEP " + stepNumber
		    + ":  EXPECTED : Should be able to set the IPV6 ping server 3 as '2003::2004'");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "Unable to set the IPV6 ping server table3 using SNMP as '2003::2004' ";
	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_THREE.getOid(), SnmpDataType.STRING,
		    BroadBandTestConstants.THIRD_IPV6_PINGSERVER_URI,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_THREE.getTableIndex());
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Successfully set the IPV6 ping server table3 using SNMP as '2003::2004'");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

	    /**
	     * STEP 13 :verify whether "PING_FAILED:1.2.3.4" message is present in self heallog.txt.0 after the setting
	     * of Invalid ping servers.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify whether 'PING_FAILED:1.2.3.4' message  is present in self heallog.txt.0  after the setting the InValid  servers.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute command : grep -i \"PING_FAILED:1.2.3.4\" /rdklogs/logs/SelfHeal.txt.0");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED - 'PING_FAILED:1.2.3.4' failure message  should  get logged in the next self heal window");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "'PING_FAILED:1.2.3.4' failure message  is not logged in the next self heal window";
	    try {
		status = BroadBandCommonUtils
			.searchLogFiles(tapEnv, device, BroadBandTraceConstants.PING_SERVER_IPV4_FIRST_URI_FAIL,
				BroadBandCommandConstants.FILE_SELFHEAL_LOG,
				BroadBandTestConstants.THIRTY_MINUTES_IN_MILLIS
					+ BroadBandTestConstants.TWENTY_MINUTES_IN_MILLIS,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)
			.contains(BroadBandTraceConstants.PING_SERVER_IPV4_FIRST_URI_FAIL.replace("\"", ""));
	    } catch (TestException exception) {
		errorMessage = exception.getMessage();
		LOGGER.error("Error occurred while searching for 'PING_FAILED:1.2.3.4' message" + errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : PING_FAILED:1.2.3.4' failure message is logged in the next self heal window");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

	    /**
	     * STEP 14 :verify whether "PING_FAILED:2.3.4.5" message is present in self heallog.txt.0 after the setting
	     * the Invalid ping servers.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify whether 'PING_FAILED:2.3.4.5' message  is present in self heallog.txt.0  after the setting the InValid  servers.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute command : grep -i \"PING_FAILED:2.3.4.5\" /rdklogs/logs/SelfHeal.txt.0");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED - 'PING_FAILED:2.3.4.5' failure message  should  get logged in the next self heal window");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "'PING_FAILED:2.3.4.5' failure message  is not logged in the next self heal window";
	    try {
		status = BroadBandCommonUtils
			.searchLogFiles(tapEnv, device, BroadBandTraceConstants.PING_SERVER_IPV4_SECOND_URI_FAIL,
				BroadBandCommandConstants.FILE_SELFHEAL_LOG, BroadBandTestConstants.FIVE_MINUTES,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)
			.contains(BroadBandTraceConstants.PING_SERVER_IPV4_SECOND_URI_FAIL.replace("\"", ""));
	    } catch (TestException exception) {
		errorMessage = exception.getMessage();
		LOGGER.error("Error occurred while searching for 'PING_FAILED:2.3.4.5' message" + errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : PING_FAILED:2.3.4.5' failure message is logged in the next self heal window");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

	    /**
	     * STEP 15 : Verify whether 'PING_FAILED:3.4.5.6' message is present in self heallog.txt.0 after the setting
	     * the InValid servers.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify whether 'PING_FAILED:3.4.5.6' message  is present in self heallog.txt.0  after the setting the InValid  servers.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute command : grep -i \"PING_FAILED:3.4.5.6\" /rdklogs/logs/SelfHeal.txt.0");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED - 'PING_FAILED:3.4.5.6' failure message  should  get logged in the next self heal window");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "'PING_FAILED:3.4.5.6' failure message  is not logged in the next self heal window";
	    try {
		status = BroadBandCommonUtils
			.searchLogFiles(tapEnv, device, BroadBandTraceConstants.PING_SERVER_IPV4_THIRD_URI_FAIL,
				BroadBandCommandConstants.FILE_SELFHEAL_LOG, BroadBandTestConstants.FIVE_MINUTES,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)
			.contains(BroadBandTraceConstants.PING_SERVER_IPV4_THIRD_URI_FAIL.replace("\"", ""));
	    } catch (TestException exception) {
		errorMessage = exception.getMessage();
		LOGGER.error("Error occurred while searching for 'PING_FAILED:3.4.5.6' message" + errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : PING_FAILED:3.4.5.6' failure message is logged in the next self heal window");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

	    /**
	     * STEP 16 :verify whether "PING_FAILED:2001:2002" message is present in self heallog.txt.0 after the
	     * setting the InValid servers.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify whether 'PING_FAILED:2001:2002' message  is present in self heallog.txt.0  after the setting the InValid  servers.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute command : grep -i \"PING_FAILED:2001:2002\" /rdklogs/logs/SelfHeal.txt.0");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED :'PING_FAILED:2001:2002' failure message  should  get logged in the next self heal window");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "'PING_FAILED:2001:2002' failure message  is not logged in the next self heal window";
	    try {
		status = BroadBandCommonUtils
			.searchLogFiles(tapEnv, device, BroadBandTraceConstants.PING_SERVER_IPV6_FIRST_URI_FAIL,
				BroadBandCommandConstants.FILE_SELFHEAL_LOG, BroadBandTestConstants.FIVE_MINUTES,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)
			.contains(BroadBandTraceConstants.PING_SERVER_IPV6_FIRST_URI_FAIL.replace("\"", ""));
	    } catch (TestException exception) {
		errorMessage = exception.getMessage();
		LOGGER.error("Error occurred while searching for 'PING_FAILED:2001:2002' message" + errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : PING_FAILED:2001:2002' failure message is logged in the next self heal window");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

	    /**
	     * STEP 17 :verify whether "PING_FAILED:2002:2003" message is present in self heallog.txt.0 after the
	     * setting the InValid servers.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : verify whether 'PING_FAILED:2002:2003' message  is present in self heallog.txt.0  after the setting the InValid  servers.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute command : grep -i \"PING_FAILED:2002:2003\" /rdklogs/logs/SelfHeal.txt.0");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED - 'PING_FAILED:2002:2003' failure message  should  get logged in the next self heal window");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "'PING_FAILED:2002:2003' failure message  is not logged in the next self heal window";
	    try {
		status = BroadBandCommonUtils
			.searchLogFiles(tapEnv, device, BroadBandTraceConstants.PING_SERVER_IPV6_SECOND_URI_FAIL,
				BroadBandCommandConstants.FILE_SELFHEAL_LOG, BroadBandTestConstants.FIVE_MINUTES,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)
			.contains(BroadBandTraceConstants.PING_SERVER_IPV6_SECOND_URI_FAIL.replace("\"", ""));
	    } catch (TestException exception) {
		errorMessage = exception.getMessage();
		LOGGER.error("Error occurred while searching for 'PING_FAILED:2002:2003' message" + errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : PING_FAILED:2002:2003' failure message is logged in the next self heal window");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

	    /**
	     * STEP 18 :verify whether "PING_FAILED:2003:2004" message is present in self heallog.txt.0 after the
	     * setting the InValid servers.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION :verify whether 'PING_FAILED:2003:2004' message  is present in self heallog.txt.0  after the setting the InValid  servers.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute command : grep -i \"PING_FAILED:2003:2004\" /rdklogs/logs/SelfHeal.txt.0");
	    LOGGER.info("STEP " + stepNumber
		    + " EXPECTED - 'PING_FAILED:2002:2003' failure message  should  get logged in the next self heal window");
	    LOGGER.info("******************************************************************************");
	    ;
	    errorMessage = "'PING_FAILED:2003:2004' failure message  is not logged in the next self heal window";
	    try {
		status = BroadBandCommonUtils
			.searchLogFiles(tapEnv, device, BroadBandTraceConstants.PING_SERVER_IPV6_THIRD_URI_FAIL,
				BroadBandCommandConstants.FILE_SELFHEAL_LOG, BroadBandTestConstants.FIVE_MINUTES,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)
			.contains(BroadBandTraceConstants.PING_SERVER_IPV6_THIRD_URI_FAIL.replace("\"", ""));
	    } catch (TestException exception) {
		errorMessage = exception.getMessage();
		LOGGER.error("Error occurred while searching for 'PING_FAILED:2003:2004' message" + errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : PING_FAILED:2003:2004' failure message is logged in the next self heal window");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

	    /**
	     * STEP 19 :verify whether "Ping failure" message is present in rdklogs/logs/selfheallog.txt.0 after the
	     * setting the Invalid servers.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify whether 'Ping failure' message  is present in rdklogs/logs/selfheallog.txt.0 after the setting the Invalid  servers.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute command :grep -i \"Ping to both IPv4 and IPv6 servers are failed\" /rdklogs/logs/SelfHeal.txt.0\".");
	    LOGGER.info("EXPECTED - Ping Failure Log  message  should  get logged in the next self heal window");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "'Ping failure' log message is not logged in the next self heal window";
	    try {
		status = BroadBandCommonUtils
			.searchLogFiles(tapEnv, device, BroadBandTraceConstants.PING_SERVER_FAIL_LOG_MESSAGE,
				BroadBandCommandConstants.FILE_SELFHEAL_LOG, BroadBandTestConstants.FIVE_MINUTES,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)
			.contains(BroadBandTraceConstants.PING_SERVER_FAIL_LOG_MESSAGE.replace("\"", ""));
	    } catch (TestException exception) {
		errorMessage = exception.getMessage();
		LOGGER.error("Error occurred while searching for 'Ping failure'  message" + errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Ping failure' log message is successful in the next self heal window");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

	    /**
	     * S20)Verify whether 'PING_FAILED:6' message is present in rdklogs/logs/dcmscript.log after getting the
	     * ping failure log messages
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION :Verify whether 'PING_FAILED:6' message  is present in rdklogs/logs/dcmscript.log after getting the ping failure log messages");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute the command  grep -i \"PING_FAILED\":\"6\" /rdklogs/logs/dcmscript.log");
	    LOGGER.info("EXPECTED - PING_FAILED:6  marker  should  get logged in the rdklogs/logs/dcmscript.log");
	    LOGGER.info("GOING TO SEARCH THE DCMSCRIPT.LOG FOR TELEMETRY MARKER FOR PING FAILURE MESSAGE");
	    LOGGER.info("******************************************************************************");
	    if (!isTelemetryEnabled) {
		JSONObject telemetryPayloadData = BroadBandTelemetryUtils.getPayLoadDataAsJson(tapEnv, device,
			BroadBandTraceConstants.TELEMETRY_MARKER_FOR_PING_SERVER_FAILURE, true);
		LOGGER.info("SEARCHED THE DCMSCRIPT.LOG FOR TELEMETRY MARKER FOR PING FAILURE MESSAGE: "
			+ (null != telemetryPayloadData));
		errorMessage = "Telemetry Marker for Ping Failure is NOT present.";
		if (null != telemetryPayloadData) {
		    String response = BroadBandTelemetryUtils.getPayloadParameterValue(telemetryPayloadData,
			    BroadBandTraceConstants.TELEMETRY_MARKER_FOR_PING_SERVER_FAILURE);
		    LOGGER.info("response is" + response);
		    status = CommonMethods.isNotNull(response) && (BroadBandCommonUtils.compareValues("INT_COMPARISON",
			    BroadBandTestConstants.STRING_VALUE_SIX, response));
		    errorMessage = "Telemetry Marker for Ping Failure is  present BUT with inappropriate value";
		}
		if (!status) {
		    errorMessage = "Telemetry Marker for Ping Failure 4 & 2 is not present in dcmscript log";
		    status = BroadBandTelemetryUtils.verifyPingFailedPayloadParamFromDcmScript(tapEnv, device,
			    BroadBandTraceConstants.TELEMETRY_MARKER_FOR_PING_SERVER_FAILURE);
		}
		if (status) {
		    LOGGER.info(
			    "STEP " + stepNumber + " : ACTUAL : Telemetry marker for ping failures is been present");
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
	    } else {
		errorMessage = "The count of PING_FAILED is less than 6";
		int count = getTextCount(tapEnv, device,
			BroadBandTraceConstants.TELEMETRY_MARKER_FOR_PING_SERVER_FAILURE,
			BroadBandCommandConstants.FILE_SELFHEAL_LOG, BroadBandTestConstants.FIVE_MINUTES,
			BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		if (count >= BroadBandTestConstants.CONSTANT_6) {
		    status = true;
		}
		if (status) {
		    LOGGER.info("STEP " + stepNumber
			    + " : ACTUAL : For Telemetry 2.0 PING_FAILED count present in selfheal.log is :" + count);
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
	    }
	    LOGGER.info("******************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

	}

	catch (Exception testException) {
	    errorMessage = "Exception occured while setting the ping servers by snmp " + testException.getMessage();
	    LOGGER.error(errorMessage);
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);
	} finally {
	    int postCondition = 0;
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");

	    /**
	     * POST-CONDITION 1: Clear/Reset the table row of ipv4 by adding a single table row
	     */
	    postCondition++;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info(
		    "POST-CONDITION 1: DESCRIPTION : Clear/Reset the  table row of ipv4 by adding a single table row");
	    LOGGER.info("POST-CONDITION : ACTION : Verify adding a table row as a ipv4 single server");
	    LOGGER.info("POST-CONDITION :EXPECTED: Should be able to add the add  table as a ipv4 single server");
	    LOGGER.info("#####################################################################################");
	    pingServersTable.clear();
	    pingServers.clear();
	    tableRowipv4.clear();
	    tableRowipv6.clear();
	    pingServers.add(BroadBandTestConstants.THIRD_IPV4_PINGSERVER_URI);
	    pingServersTable.put(BroadBandWebPaConstants.WEBPA_IPV4_PING_SERVER_URI, pingServers);
	    webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
		    BroadBandWebPaConstants.WEBPA_IPV4_PING_ADD_TABLE, pingServersTable);
	    tableRowNumber = webPaServerResponse.getRow();
	    tableRowipv4.add(tableRowNumber);
	    status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
	    if (status) {
		LOGGER.info(
			"POST-CONDITION " + postCondition + ": ACTUAL : Added a single table row for Ipv4 ping server");
	    } else {
		LOGGER.error("POST-CONDITION " + postCondition + ": ACTUAL : Post condition failed");
	    }

	    /**
	     * POST-CONDITION 2: Clear/Reset the table row of ipv6 by adding a single table row
	     */
	    postCondition++;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("POST-CONDITION " + postCondition
		    + ": DESCRIPTION : Clear/Reset the  table row of ipv6 by adding a single table row");
	    LOGGER.info("POST-CONDITION " + postCondition
		    + ": : ACTION : Verify adding a table row as a ipv6 single server");
	    LOGGER.info("POST-CONDITION " + postCondition
		    + ":  :EXPECTED: Should be able to add the add  table as a ipv6 single server");
	    LOGGER.info("#####################################################################################");
	    pingServersTable.clear();
	    pingServers.clear();
	    pingServers.add(BroadBandTestConstants.THIRD_IPV6_PINGSERVER_URI);
	    pingServersTable.put(BroadBandWebPaConstants.WEBPA_IPV6_PING_SERVER_URI, pingServers);
	    webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
		    BroadBandWebPaConstants.WEBPA_IPV6_PING_ADD_TABLE, pingServersTable);
	    tableRowNumber = webPaServerResponse.getRow();
	    tableRowipv6.add(tableRowNumber);
	    status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
	    if (status) {
		LOGGER.info(
			"POST-CONDITION " + postCondition + ": ACTUAL : Added a single table row for Ipv6 ping server");
	    } else {
		LOGGER.error("POST-CONDITION " + postCondition + ": ACTUAL : Post condition failed");
	    }

	    /**
	     * POST-CONDITION 3: Delete the added ipv4 ping servers in the Ping Server Table by using WEBPA DELETE
	     * command.
	     */
	    postCondition++;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("POST-CONDITION " + postCondition
		    + ": DESCRIPTION :Delete the added ipv4 ping servers in the Ping Server Table by using WEBPA DELETE command.");
	    LOGGER.info("POST-CONDITION " + postCondition
		    + ": ACTION : Delete the added Ping Servers in the ipv4 Ping Server Table by using WEBPA DELETE command");
	    LOGGER.info("POST-CONDITION " + postCondition
		    + ": EXPECTED: Should be able to delete the added ipv4 ping servers in the Ping Server Table using webpa Delete command");
	    LOGGER.info("#####################################################################################");
	    String num = CommonMethods.patternFinder(tableRowipv4.get(0),
		    BroadBandTestConstants.PATTERN_MATCHER_TO_GET_TABLE_ROWS);
	    for (int count = 1; count <= Integer.parseInt(num); count++) {
		webPaServerResponse = tapEnv.deleteTableRowUsingRestApi(device,
			"Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable." + count + ".");
		status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
		if (status) {
		    LOGGER.info("POST-CONDITION " + postCondition + ": ACTUAL : Deleted ping servers table row="
			    + "Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable." + count + "");
		} else {
		    LOGGER.error("POST-CONDITION " + postCondition + ": ACTUAL : Post condition failed");
		}
	    }

	    /**
	     * POST-CONDITION 4: Delete the added ipv6 ping servers in the Ping Server Table by using WEBPA DELETE
	     * command.
	     */
	    postCondition++;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("POST-CONDITION " + postCondition
		    + ": DESCRIPTION : Delete the added ipv6 ping servers in the Ping Server Table by using WEBPA DELETE command.");
	    LOGGER.info("POST-CONDITION " + postCondition
		    + ": ACTION : Delete the added Ping Servers in the ipv6 Ping Server Table by using WEBPA DELETE command");
	    LOGGER.info("POST-CONDITION " + postCondition
		    + ": EXPECTED: Should be able to delete the added ipv6 ping servers in the Ping Server Table using webpa Delete command");
	    LOGGER.info("#####################################################################################");
	    num = CommonMethods.patternFinder(tableRowipv6.get(0),
		    BroadBandTestConstants.PATTERN_MATCHER_TO_GET_TABLE_ROWS);
	    for (int count = 1; count <= Integer.parseInt(num); count++) {
		webPaServerResponse = tapEnv.deleteTableRowUsingRestApi(device,
			"Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable." + count + ".");
		status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
		if (status) {
		    LOGGER.info("POST-CONDITION " + postCondition
			    + ": ACTUAL : Deleted ping servers table row for ipv6="
			    + "Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable." + count + "");
		} else {
		    LOGGER.error("POST-CONDITION " + postCondition + ": ACTUAL : Post condition failed");
		}
	    }
	    /**
	     * POST-CONDITION 5: Begin clear telemetry configuration on the device.
	     */
	    postCondition++;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("POST-CONDITION " + postCondition
		    + ": DESCRIPTION : Begin clear telemetry configuration on the device.");
	    LOGGER.info("POST-CONDITION " + postCondition + ":  ACTION : Clear telemetry configuration on the device");
	    LOGGER.info("POST-CONDITION " + postCondition
		    + ":  EXPECTED: Should be able to clear the telemetry configuration on the device");
	    LOGGER.info("#######################################################################################");
	    boolean result = BroadBandTelemetryUtils.clearTelemetryConfiguration(tapEnv, device);
	    if (result) {
		LOGGER.info("POST-CONDITION " + postCondition
			+ ": ACTUAL : End clear telemetry configuration on the device");
	    } else {
		LOGGER.error("POST-CONDITION " + postCondition + ": ACTUAL : Post condition failed");
	    }

	    /**
	     * POST-CONDITION 6: Begin re-activation operation on the device
	     */
	    postCondition++;
	    BroadBandPostConditionUtils.executePostConditionToReActivateDevice(device, tapEnv, false, postCondition);

	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");

	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-NW-CONNECTIVITY-1006");
    }

    /**
     * 
     * TC-RDKB-NW-CONNECTIVITY-1008: verify the Configuration of ping packet size via WebPA and also in device logs
     *
     * <ol>
     * <li>S1)Verify and setting the datablock size as "64" using the TR-181
     * parameter-Device.IP.Diagnostics.IPPing.DataBlockSize.</li>
     * <li>S2)verify whether the ping DatablockSize is getting logged in /opt/secure/data/syscfg.db file</li>
     * </ol>
     *
     * @author Joseph Maduram
     * @refactor Govardhan
     * 
     * @param device
     *            {@link Dut}
     */

    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    TestGroup.WEBPA }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-NW-CONNECTIVITY-1008")

    public void TestPingDataPacketSize(Dut device) {
	// String to store the test case ID
	String testId = "TC-RDKB-NW-CONNECTIVITY-108";
	// String to store the test step number
	String testStepNumber = "s1";
	// String to store the error message
	String errorMessage = null;
	// stores the test status
	boolean status = false;
	// String to store the response
	String response = null;
	// String to store the dataBlockSize
	String dataBlockSize = null;
	try {

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "PRE-CONDITION 1:DESCRIPTION : Get the DatablockSize from the device by using WebPA parameter 'Device.IP.Diagnostics.IPPing.DataBlockSize'");
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("PRE-CONDITION : ACTION : Get the DatablockSize from the device by using WebPA");
	    LOGGER.info("PRE-CONDITION : EXPECTED : should be able to get the datablock size");
	    dataBlockSize = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_DATABLOCKSIZE);
	    if (CommonMethods.isNull(dataBlockSize)) {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
			+ "DATA BLOCK SIZE IS NULL - HENCE BLOCKING THE EXECUTION");
	    }
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-NW-CONNECTIVITY-1008");
	    LOGGER.info(
		    "TEST DESCRIPTION: verify whether the device reboots when there is a Log ping miss in Self heal log.");
	    LOGGER.info("#######################################################################################");

	    /**
	     * STEP 1:Verify and setting the datablock size as "64" using the TR-181
	     * parameter-Device.IP.Diagnostics.IPPing.DataBlockSize.
	     * 
	     */
	    testStepNumber = "s1";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 1 : verify and setting the datablocksize as '64' using the TR-181 parameter-Device.IP.Diagnostics.IPPing.DataBlockSize.");
	    LOGGER.info("STEP 1 : EXPECTED: should be able to set the Data block Size  as '64'by WebPA");
	    LOGGER.info("#####################################################################################");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_DATABLOCKSIZE, BroadBandTestConstants.CONSTANT_2,
		    BroadBandTestConstants.DATA_BLOCK_SIZE);
	    errorMessage = "Unable to set the  data block size  'Device.IP.Diagnostics.IPPing.DataBlockSize' as '64' using WebPA command.";
	    LOGGER.info("STEP 1 : ACTUAL:" + (status
		    ? "Successfully set the  data block size  'Device.IP.Diagnostics.IPPing.DataBlockSize' as '64' using WebPA command."
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 2:verify whether the ping DatablockSize is getting logged in /opt/secure/data/syscfg.db file
	     * 
	     */
	    status = false;
	    testStepNumber = "s2";
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 2 : verify whether the  ping DatablockSize is getting logged in /opt/secure/data/syscfg.db file");
	    LOGGER.info("STEP 2 : EXPECTED: should be able to get datablocksize by WebPA");
	    LOGGER.info("#####################################################################################");

	    status = BroadBandCommonUtils
		    .searchLogFiles(tapEnv, device, BroadBandTraceConstants.DATA_BLOCK_SIZE_LOG_MESSAGE,
			    BroadBandCommandConstants.LOG_FILE_SECURE_SYSCFG,
			    BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)
		    .contains(BroadBandTraceConstants.DATA_BLOCK_SIZE_LOG_MESSAGE.replace("\"", ""));

	    errorMessage = "ping datablockSize is not logged in /opt/secure/data/syscfg.db file";
	    LOGGER.info("STEP 2 :  ACTUAL:"
		    + (status ? "ping DatablockSize is logged in /opt/secure/data/syscfg.db file" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	}

	catch (Exception exception) {
	    status = false;
	    errorMessage = "Exception occurred while  checking the data Block Size" + exception.getMessage();
	    LOGGER.error(errorMessage);
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
	} finally {

	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "POST CONDITION : Check and Set the  'Device.IP.Diagnostics.IPPing.DataBlockSize' to the pre-existing value using WebPA.");
	    LOGGER.info("#####################################################################################");
	    if (!BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_DATABLOCKSIZE, BroadBandTestConstants.CONSTANT_2, dataBlockSize)) {
		throw new TestException(
			"NOT ABLE TO SET THE DATABLOCKSIZE  TO THE PRE-EXISTING VALUE USING WEBPA - HENCE BLOCKING THE EXECUTION");
	    }

	}

	/**
	 * STEP 3:verify the presence /nvram/syscfg.db file in ATOM Side
	 */
	status = false;
	testStepNumber = "s3";
	LOGGER.info("#####################################################################################");
	LOGGER.info("STEP 3 : verify the presence /nvram/syscfg.db file in ATOM Side");
	LOGGER.info("STEP 3 : EXPECTED: should be able to get /nvram/syscfg.db in ATOM side ");
	LOGGER.info("#####################################################################################");

	if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
	    String command = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CAT_COMMAND,
		    BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandCommandConstants.LOG_FILE_SYSCFG);
	    response = CommonMethods.executeCommandInAtomConsole(device, tapEnv, command);

	    LOGGER.info("COMMAND EXECUTION RESPONSE: " + response);

	    if (status = CommonMethods.isNotNull(response)
		    & !response.contains(BroadBandTestConstants.NO_SUCH_FILE_OR_DIRECTORY)) {

		LOGGER.info("STEP 3 : Actual : SUCCESSFULLY VERIFIED syscfg.db FILE ON ATOM CONSOLE");
	    } else {
		errorMessage = "syscfg.db FILE IS NOT EXISTED ON ATOM CONSOLE";
		LOGGER.error(errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	} else {
	    LOGGER.info("STEP 3 is applicable only for Atom Sync Available Devices..Not applicable for  "
		    + device.getModel() + "  Model !!!");
	    tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
		    errorMessage, false);
	}
    }

    /**
     * 
     * 
     * TC-RDKB-NW-CONNECTIVITY-1009: Verify whether the Invalid ping servers and valid ping servers are set and it's
     * failure messages are logged when the ping fails in selfHeal log
     * <ol>
     * <li>S1)Verification of adding Invalid Ipv4 ping server '1.2.3.4' by using Webpa POST command on TR-181 parameter
     * 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.'</li>
     * <li>S2)Verification of adding Invalid Ipv4 ping server '2.3.4.5' by using Webpa POST command on TR-181 parameter
     * 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.'</li>
     * <li>S3)Verification of adding valid Ipv4 ping server in table row 3 by using Webpa POST command on
     * "Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.</li>
     * <li>S4)Verification of adding Invalid Ipv6 ping server '2001::2002' by using Webpa POST command on TR-181
     * parameter 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.'</li>
     * <li>S5)Verification of adding Invalid Ipv6 ping server '2002::2003' by using Webpa POST command on TR-181
     * parameter 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.'</li>
     * <li>S6)Verification of adding valid Ipv6 ping server in table row 3 by using Webpa POST command on
     * "Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.</li>
     * <li>S7)Verify whether 'PING_FAILED:1.2.3.4' message is present in self heallog.txt.0 after the setting the
     * InValid servers.</li>
     * <li>S8)verify whether 'PING_FAILED:2.3.4.5' message is present in self heallog.txt.0 after the setting the
     * InValid servers.</li>
     * <li>S9)verify whether 'PING_FAILED:2001::2002' message is present in self heallog.txt.0 after the setting the
     * InValid servers.</li>
     * <li>S10)verify whether 'PING_FAILED:2002::2003' message is present in self heallog.txt.0 after the setting the
     * InValid servers.</li>
     * <li>S11)verify whether 'Connectivity successfull' message is present in rdklogs/logs/selfheallog.txt.0 after the
     * setting the Invalid servers.</li>
     * </ol>
     *
     * @author Joseph Maduram
     * @refactor Govardhan
     * @param device
     *            {@link Dut}
     */

    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    BroadBandTestGroup.WEBPA }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-NW-CONNECTIVITY-1009")
    public void invalidAndValidPingServerConnectivityTestByWebpa(Dut device) {
	// String to store the test case ID
	String testId = "TC-RDKB-NW-CONNECTIVITY-109";
	// String to store the test step number
	String testStepNumber = "s1";
	// String to store the error message
	String errorMessage = null;
	// stores the test status
	boolean status = false;
	// String to store the added table row number
	String tableRowNumber = null;
	// List of String to store the added table row numbers
	List<String> tableRow = new ArrayList<String>();
	// string to store the webpaserver response
	WebPaServerResponse webPaServerResponse = null;
	// Map of string and List for Ping table
	Map<String, List<String>> pingServersTable = new HashMap<String, List<String>>();
	// List of String to store the ping servers
	List<String> pingServers = new ArrayList<String>();
	String ipv4PingServer = null;
	String ipv6PingServer = null;
	try {

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "PRE-CONDITION 1:DESCRIPTION :  Check and Set the Ping Interval to 15 mins 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval'using WebPA.");
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("PRE-CONDITION : ACTION : Set the ping Interval to 15 mins using WebPA");
	    LOGGER.info("PRE-CONDITION : EXPECTED : ping Interval should be set to 15 mins");
	    if (!BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.PING_INTERVAL, BroadBandTestConstants.CONSTANT_2,
		    BroadBandTestConstants.CONSTANT_PING_INTERVAL)) {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
			+ "NOT ABLE TO SET THE PING INTERVAL TO 15 MINS - HENCE BLOCKING THE EXECUTION");
	    }
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-NW-CONNECTIVITY-1009");
	    LOGGER.info(
		    "TEST DESCRIPTION: Verify whether the invalid  ping servers and valid ping servers are set by webpa and  is failure messages are logged when the ping fails in selfHeal log.");
	    LOGGER.info("#######################################################################################");

	    /**
	     * STEP 1:Verification of adding Invalid Ipv4 ping server '1.2.3.4' by using Webpa POST command on TR-181
	     * parameter 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.'
	     * 
	     */
	    testStepNumber = "s1";
	    status = false;
	    String timeStamp = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 1 : Verification of adding Invalid Ipv4 ping server '1.2.3.4' by using Webpa POST command on TR-181 parameter 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.'");
	    LOGGER.info("STEP 1 : EXPECTED: should be able to set the IPV4 ping server1  as '1.2.3.4'by WebPA");
	    LOGGER.info("#####################################################################################");
	    pingServers.add(BroadBandTestConstants.FIRST_IPV4_PINGSERVER_URI);
	    pingServersTable.put(BroadBandWebPaConstants.WEBPA_IPV4_PING_SERVER_URI, pingServers);
	    webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
		    BroadBandWebPaConstants.WEBPA_IPV4_PING_ADD_TABLE, pingServersTable);
	    tableRowNumber = webPaServerResponse.getRow();
	    tableRow.add(tableRowNumber);
	    status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
	    errorMessage = "Unable to set the   IPV4 ping server URI  'Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.1.X_RDKCENTRAL-COM_Ipv4PingServerURI' valid Ipv4 as '1.2.3.4' using WebPA command.";
	    LOGGER.info("STEP 1 : ACTUAL : " + (status
		    ? "Successfully set the  IPV4 ping server URI  'Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.1.X_RDKCENTRAL-COM_Ipv4PingServerURI' valid Ipv4 as '1.2.3.4' using WebPA command."
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 2:Verification of adding Invalid Ipv4 ping server '2.3.4.5' by using Webpa POST command on TR-181
	     * parameter 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.'
	     * 
	     */

	    testStepNumber = "s2";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 2 : Verification of adding Invalid Ipv4 ping server '2.3.4.5' by using Webpa POST command on TR-181 parameter 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.'");
	    LOGGER.info("STEP 2 : EXPECTED: should be able to set the IPV4 ping server2  as '2.3.4.5'by WebPA");
	    LOGGER.info("#####################################################################################");
	    pingServersTable.clear();
	    pingServers.clear();
	    pingServers.add(BroadBandTestConstants.SECOND_IPV4_PINGSERVER_URI);
	    pingServersTable.put(BroadBandWebPaConstants.WEBPA_IPV4_PING_SERVER_URI, pingServers);
	    webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
		    BroadBandWebPaConstants.WEBPA_IPV4_PING_ADD_TABLE, pingServersTable);
	    tableRowNumber = webPaServerResponse.getRow();
	    tableRow.add(tableRowNumber);
	    status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
	    errorMessage = "Unable to set the  IPV4 ping server URI  'Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.2.X_RDKCENTRAL-COM_Ipv4PingServerURI' Invalid Ipv4 as '2.3.4.5' using WebPA command.";
	    LOGGER.info("STEP 2 : ACTUAL : " + (status
		    ? "Successfully set the  IPV4 ping server URI  'Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.2.X_RDKCENTRAL-COM_Ipv4PingServerURI' Invalid Ipv4 as '2.3.4.5' using WebPA command."
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 3:Verification of adding valid Ipv4 ping server in table row 3 by using Webpa POST command on
	     * "Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable."
	     * 
	     */
	    testStepNumber = "s3";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 3 : Verification of adding valid Ipv4 ping server in table row 1 by using  Webpa POST command on 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.'");
	    LOGGER.info("STEP 3 : EXPECTED: should be able to set the IPV4 ping server1  by WebPA");
	    LOGGER.info("#####################################################################################");
	    pingServersTable.clear();
	    pingServers.clear();
	    ipv4PingServer = BroadBandNetworkConnectivityUtils.resolvePingServer(device, tapEnv,
		    BroadBandTestConstants.IP_VERSION4);
	    pingServers.add(ipv4PingServer);
	    pingServersTable.put(BroadBandWebPaConstants.WEBPA_IPV4_PING_SERVER_URI, pingServers);
	    webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
		    BroadBandWebPaConstants.WEBPA_IPV4_PING_ADD_TABLE, pingServersTable);
	    tableRowNumber = webPaServerResponse.getRow();
	    tableRow.add(tableRowNumber);
	    status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
	    errorMessage = "Unable to set the  third IPV4 ping server URI with valid Ipv4 ping server using WebPA command.";
	    LOGGER.info("STEP 3 : ACTUAL : " + (status
		    ? "Successfully added the valid Ipv4 ping server as  third IPV4 ping server URI using WebPA command."
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 4:Verification of adding Invalid Ipv6 ping server '2001::2002' by using Webpa POST command on TR-181
	     * parameter 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.'
	     * 
	     */

	    testStepNumber = "s4";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 4 : Verification of adding Invalid Ipv6 ping server '2001::2002' by using Webpa POST command on TR-181 parameter 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.'");
	    LOGGER.info("STEP 4 : EXPECTED: should be able to set the IPV6 ping server1  as '2001::2002'by WebPA");
	    LOGGER.info("#####################################################################################");
	    pingServersTable.clear();
	    pingServers.clear();
	    pingServers.add(BroadBandTestConstants.FIRST_IPV6_PINGSERVER_URI);
	    pingServersTable.put(BroadBandWebPaConstants.WEBPA_IPV6_PING_SERVER_URI, pingServers);
	    webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
		    BroadBandWebPaConstants.WEBPA_IPV6_PING_ADD_TABLE, pingServersTable);
	    tableRowNumber = webPaServerResponse.getRow();
	    tableRow.add(tableRowNumber);
	    status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
	    errorMessage = "Unable to set the IPV6 ping server URI  'Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.1.X_RDKCENTRAL-COM_Ipv6PingServerURI' Invalid Ipv6 as '2001::2002' using WebPA command.";
	    LOGGER.info("STEP 4 : ACTUAL : " + (status
		    ? "Successfully set the IPV6 ping server URI  'Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.1.X_RDKCENTRAL-COM_Ipv6PingServerURI' Invalid Ipv6 as '2001::2002' using WebPA command."
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 5:Verification of adding Invalid Ipv6 ping server '2002::2003' by using Webpa POST command on TR-181
	     * parameter 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.'
	     * 
	     */

	    testStepNumber = "s5";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 5 : Verification of adding Invalid Ipv6 ping server '2002::2003' by using Webpa POST command on TR-181 parameter 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.'");
	    LOGGER.info("STEP 5 : EXPECTED: should be able to set the IPV6 ping server2  as '2002::2003'by WebPA");
	    LOGGER.info("#####################################################################################");
	    pingServersTable.clear();
	    pingServers.clear();
	    pingServers.add(BroadBandTestConstants.SECOND_IPV6_PINGSERVER_URI);
	    pingServersTable.put(BroadBandWebPaConstants.WEBPA_IPV6_PING_SERVER_URI, pingServers);
	    webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
		    BroadBandWebPaConstants.WEBPA_IPV6_PING_ADD_TABLE, pingServersTable);
	    tableRowNumber = webPaServerResponse.getRow();
	    tableRow.add(tableRowNumber);
	    status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
	    errorMessage = "Unable to set the IPV6 ping server URI  'Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.2.X_RDKCENTRAL-COM_Ipv6PingServerURI' Invalid Ipv6 as '2002::2003' using WebPA command.";
	    LOGGER.info("STEP 5: ACTUAL : " + (status
		    ? "Successfully set the IPV6 ping server URI  'Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.2.X_RDKCENTRAL-COM_Ipv6PingServerURI' Invalid Ipv6 as '2002::2003' using WebPA command."
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 6:Verification of adding valid Ipv6 ping server in table row 3 by using Webpa POST command on
	     * "Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable."
	     * 
	     */
	    testStepNumber = "s6";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 6 : Verification of adding valid Ipv6 ping server in table row 3 by using  Webpa POST command on 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.'");
	    LOGGER.info("STEP 6 : EXPECTED: should be able to set the IPV6 ping server  by WebPA");
	    LOGGER.info("#####################################################################################");
	    pingServersTable.clear();
	    pingServers.clear();
	    ipv6PingServer = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_WAN_IPV6);
	    pingServers.add(ipv6PingServer);
	    pingServersTable.put(BroadBandWebPaConstants.WEBPA_IPV6_PING_SERVER_URI, pingServers);
	    webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
		    BroadBandWebPaConstants.WEBPA_IPV6_PING_ADD_TABLE, pingServersTable);
	    tableRowNumber = webPaServerResponse.getRow();
	    tableRow.add(tableRowNumber);
	    status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
	    errorMessage = "Unable to set the  third IPV6 ping server URI with valid Ipv6 ping server using WebPA command.";
	    LOGGER.info("STEP 6 : ACTUAL : " + (status
		    ? "Successfully added the valid Ipv6 ping server as  third IPV6 ping server URI using WebPA command."
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 7 :verify whether "PING_FAILED:1.2.3.4" message is present in self heallog.txt.0 after the setting
	     * the InValid servers.
	     */
	    testStepNumber = "s7";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 7 : Verify whether 'PING_FAILED:1.2.3.4' message  is present in self heallog.txt.0  after the setting the InValid  servers.");
	    LOGGER.info(
		    "STEP 7 : EXPECTED - 'PING_FAILED:1.2.3.4' failure message  should  get logged in the next self heal window");
	    LOGGER.info("#####################################################################################");
	    try {
		status = BroadBandCommonUtils
			.searchLogFiles(tapEnv, device, BroadBandTraceConstants.PING_SERVER_IPV4_FIRST_URI_FAIL,
				BroadBandCommandConstants.FILE_SELFHEAL_LOG,
				BroadBandTestConstants.THIRTY_MINUTES_IN_MILLIS
					+ BroadBandTestConstants.TWENTY_MINUTES_IN_MILLIS,
				BroadBandTestConstants.ONE_MINUTE_IN_MILLIS)
			.contains(BroadBandTraceConstants.PING_SERVER_IPV4_FIRST_URI_FAIL.replace("\"", ""));
	    } catch (TestException exception) {
		errorMessage = exception.getMessage();
		LOGGER.error("error occured while checking the 'PING_FAILED:1.2.3.4' message in self heallog.txt.0  "
			+ errorMessage);
	    }
	    errorMessage = "'PING_FAILED:1.2.3.4' failure message  is not logged in the next self heal window";
	    LOGGER.info("STEP 7 : ACTUAL: "
		    + (status ? " 'PING_FAILED:1.2.3.4' failure message is logged in the next self heal window"
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 8 :verify whether "PING_FAILED:2.3.4.5" message is present in self heallog.txt.0 after the setting
	     * the InValid servers.
	     */
	    testStepNumber = "s8";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 8 : verify whether 'PING_FAILED:2.3.4.5' message  is present in self heallog.txt.0  after the setting the InValid  servers.");
	    LOGGER.info(
		    "STEP 8 : EXPECTED - 'PING_FAILED:2.3.4.5' failure message  should  get logged in the next self heal window");
	    LOGGER.info("#####################################################################################");
	    try {
		status = BroadBandCommonUtils
			.searchLogFiles(tapEnv, device, BroadBandTraceConstants.PING_SERVER_IPV4_SECOND_URI_FAIL,
				BroadBandCommandConstants.FILE_SELFHEAL_LOG,
				BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)
			.contains(BroadBandTraceConstants.PING_SERVER_IPV4_SECOND_URI_FAIL.replace("\"", ""));
	    } catch (TestException exception) {
		errorMessage = exception.getMessage();
		LOGGER.error("error occured while checking the 'PING_FAILED:2.3.4.5 message in self heallog.txt.0  "
			+ errorMessage);
	    }
	    errorMessage = "'PING_FAILED:2.3.4.5' failure message  is not logged in the next self heal window";
	    LOGGER.info("STEP 8 : ACTUAL: "
		    + (status ? " 'PING_FAILED:2.3.4.5' failure message is logged in the next self heal window"
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 9 :verify whether "PING_FAILED:2001::2002" message is present in self heallog.txt.0 after the
	     * setting the InValid servers.
	     */
	    testStepNumber = "s9";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 9 : verify whether 'PING_FAILED:2001::2002' message  is present in self heallog.txt.0  after the setting the InValid  servers.");
	    LOGGER.info(
		    "STEP 9 : EXPECTED - 'PING_FAILED:2001::2002' failure message  should  get logged in the next self heal window");
	    LOGGER.info("#####################################################################################");
	    try {
		status = BroadBandCommonUtils
			.searchLogFiles(tapEnv, device, BroadBandTraceConstants.PING_SERVER_IPV6_FIRST_URI_FAIL,
				BroadBandCommandConstants.FILE_SELFHEAL_LOG,
				BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
				BroadBandTestConstants.ONE_MINUTE_IN_MILLIS)
			.contains(BroadBandTraceConstants.PING_SERVER_IPV6_FIRST_URI_FAIL.replace("\"", ""));
	    } catch (TestException exception) {
		errorMessage = exception.getMessage();
		LOGGER.error("error occured while checking the 'PING_FAILED:2001::2002 message in self heallog.txt.0  "
			+ errorMessage);
	    }
	    errorMessage = "'PING_FAILED:2001::2002' failure message  is not logged in the next self heal window";

	    LOGGER.info("STEP 9 : ACTUAL: "
		    + (status ? " 'PING_FAILED:2001:2002' failure message is logged in the next self heal window"
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 10 :verify whether "PING_FAILED:2002::2003" message is present in self heallog.txt.0 after the
	     * setting the InValid servers.
	     */
	    testStepNumber = "s10";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 10 : verify whether 'PING_FAILED:2002::2003' message  is present in self heallog.txt.0  after the setting the InValid  servers.");
	    LOGGER.info(
		    "STEP 10 : EXPECTED - 'PING_FAILED:2002::2003' failure message  should  get logged in the next self heal window");
	    LOGGER.info("#####################################################################################");
	    try {
		status = BroadBandCommonUtils
			.searchLogFiles(tapEnv, device, BroadBandTraceConstants.PING_SERVER_IPV6_SECOND_URI_FAIL,
				BroadBandCommandConstants.FILE_SELFHEAL_LOG,
				BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)
			.contains(BroadBandTraceConstants.PING_SERVER_IPV6_SECOND_URI_FAIL.replace("\"", ""));
	    } catch (TestException exception) {
		errorMessage = exception.getMessage();
		LOGGER.error(
			"error occured while checking the '2002::2003 message in self heallog.txt.0  " + errorMessage);
	    }
	    errorMessage = "'PING_FAILED:2002:2003' failure message  is not logged in the next self heal window";
	    LOGGER.info("STEP 10 : ACTUAL: "
		    + (status ? " 'PING_FAILED:2002:2003' failure message is logged in the next self heal window"
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 11 :verify whether "ping to the other list is successfull" message is present in
	     * rdklogs/logs/selfheallog.txt.0 after the setting the Invalid servers.
	     */
	    testStepNumber = "s11";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 11 : verify whether 'Connectivity successfull' message  is present in rdklogs/logs/selfheallog.txt.0 after the setting the Invalid  servers.");
	    LOGGER.info(
		    "STEP 11 : EXPECTED - Connectivity successfull Log  message  should  get logged in the next self heal window");
	    LOGGER.info("#####################################################################################");
	    try {
		status = BroadBandCommonUtils
			.searchLogFiles(tapEnv, device, BroadBandTraceConstants.PING_SERVER_SUCCESS_LOG_MESSAGE,
				BroadBandCommandConstants.FILE_SELFHEAL_LOG, BroadBandTestConstants.FIVE_MINUTES,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)
			.contains(BroadBandTraceConstants.PING_SERVER_SUCCESS_LOG_MESSAGE.replace("\"", ""));
	    } catch (TestException exception) {
		errorMessage = exception.getMessage();
		LOGGER.error(
			"error occured while checking the 'ping to the other list is successfull message in self heallog.txt.0  "
				+ errorMessage);
	    }
	    errorMessage = "'Connectivity successfull' log message is not logged in the next self heal window";
	    LOGGER.info("STEP 11 : ACTUAL: "
		    + (status ? "'Connectivity successfull' log message is successful in the next self heal window"
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	} catch (Exception testException) {
	    errorMessage = "Exception occured while checking the valid and invalid ping server connectivity test "
		    + testException.getMessage();
	    LOGGER.error(errorMessage);
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
	} finally {

	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "POST-CONDITION 1: DESCRIPTION :delete the added ping servers in the Ping Server Table by using WEBPA DELETE command.");
	    LOGGER.info(
		    "POST-CONDITION : ACTION : Delete the added Ping Servers in the Ping Server Table by using WEBPA DELETE command");
	    LOGGER.info(
		    "POST-CONDITION :EXPECTED: Should be able to delete the added ping servers in the Ping Server Table using webpa Delete command");
	    LOGGER.info("#####################################################################################");

	    for (int i = 0; i < tableRow.size(); i++) {
		webPaServerResponse = tapEnv.deleteTableRowUsingRestApi(device, tableRow.get(i));
		status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
		if (status) {
		    LOGGER.info("Deleted ping servers table row" + tableRow.get(i));
		}
	    }

	}
    }

    /**
     * 
     * TC-RDKB-NW-CONNECTIVITY-1010: Verify whether the Invalid ping servers and valid ping servers are set by SNMP and
     * it's failure messages are logged when the ping fails in selfHeal log
     *
     * <ol>
     * <li>S1)Verify adding the IPV4 ping server table 1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.3.1).</li>
     * <li>S2)Verify setting the IPV4 ping server 1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.2.1) as
     * '1.2.3.4'</li>
     * <li>S3)Verify adding the IPV4 ping server table 2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.3.2).</li>
     * <li>S4)Verify setting the IPV4 ping server 2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.2.2) as
     * '2.3.4.5'</li>
     * <li>S5)Verify adding the IPV4 ping server table 3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.3.3).</li>
     * <li>S6)Verify setting the IPV4 ping server table 3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.2.1)</li>
     * <li>S7)Verify adding the IPV6 ping server table 1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.3.1).</li>
     * <li>S8)Verify setting the IPV6 ping server 1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.2.1) as
     * '2001::2002'.</li>
     * <li>S9)Verify adding the IPV6 ping server table 2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.3.2).</li>
     * <li>S10)Verify setting the IPV6 ping server 2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.2.2) as
     * '2002::2003'</li>
     * <li>S11)Verify adding the IPV6 ping server table 3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.3.3).</li>
     * <li>S12)Verify setting the IPV6 ping server table 3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.2.1)</li>
     * <li>S13)verify whether "PING_FAILED:1.2.3.4" message is present in self heallog.txt.0 after the setting the
     * InValid servers.</li>
     * <li>S14)verify whether 'PING_FAILED:2.3.4.5' message is present in self heallog.txt.0 after the setting the
     * InValid servers.</li>
     * <li>S15)verify whether 'PING_FAILED:2001::2002' message is present in self heallog.txt.0 after the setting the
     * InValid servers.</li>
     * <li>S16)verify whether 'PING_FAILED:2002::2003' message is present in self heallog.txt.0 after the setting the
     * InValid servers.</li>
     * <li>S17)verify whether 'Connectivity successfull' message is present in rdklogs/logs/selfheallog.txt.0 after the
     * setting the Invalid servers.</li>
     * </ol>
     *
     * @author Joseph Maduram
     * @refactor Govardhan
     * 
     * @param device
     *            {@link Dut}
     */

    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    BroadBandTestGroup.WEBPA }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-NW-CONNECTIVITY-1010")

    public void invalidAndValidPingServerConnectivityTestBySnmp(Dut device) {
	// String to store the test case ID
	String testId = "TC-RDKB-NW-CONNECTIVITY-110";
	// String to store the test step number
	String testStepNumber = "s1";
	// String to store the error message
	String errorMessage = null;
	// stores the test status
	boolean status = false;
	// string to store the webpaserver response
	WebPaServerResponse webPaServerResponse = null;
	String ipv4PingServer = null;
	String ipv6PingServer = null;
	String tableRowNumber = null;
	// Map of string and List for Ping table
	Map<String, List<String>> pingServersTable = new HashMap<String, List<String>>();
	// List of String to store the ping servers
	List<String> pingServers = new ArrayList<String>();
	// List of String to store the added table row numbers
	List<String> tableRowipv4 = new ArrayList<String>();
	List<String> tableRowipv6 = new ArrayList<String>();
	String snmpSetResponse = null;

	try {

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "PRE-CONDITION 1:DESCRIPTION :  Check and Set the Ping Interval to 15 mins 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval'using WebPA.");
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("PRE-CONDITION : ACTION : Set the ping Interval to 15 mins using WebPA");
	    LOGGER.info("PRE-CONDITION : EXPECTED : ping Interval should be set to 15 mins");
	    // If last (third) parameter in verifyWebPaProcessIsUp is set to true, this
	    // command could
	    // potentially run for 8 minutes. As false, it will run no longer than 30
	    // seconds.
	    if (BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, false)) {
		LOGGER.info("WEBPA IS UP AND RUNNING - CONTINUING THE EXECUTION");
	    } else {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
			+ "WEBPA IS NOT UP AND RUNNING - HENCE BLOCKING THE EXECUTION");
	    }
	    if (!BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.PING_INTERVAL, BroadBandTestConstants.CONSTANT_2,
		    BroadBandTestConstants.CONSTANT_PING_INTERVAL)) {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
			+ "NOT ABLE TO SET THE PING INTERVAL TO 15 MINS - HENCE BLOCKING THE EXECUTION");
	    }
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-NW-CONNECTIVITY-1010");
	    LOGGER.info(
		    "TEST DESCRIPTION: Verify whether the invalid  ping servers and valid ping servers are set by SNMP and  is failure messages are logged when the ping fails in selfHeal log.");
	    LOGGER.info("#######################################################################################");

	    /**
	     * STEP 1:Verify adding the IPV4 ping server table 1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.3.1).
	     * 
	     */
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    testStepNumber = "s1";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION: Verify adding the IPV4 ping server table 1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.3.1).");
	    LOGGER.info(
		    "STEP 1: ACTION: Execute the below snmp command to add the  IPV4 ping server 1 table, snmpset -v 2c -c string1 10.254.35.35 .1.3.6.1.4.1.17270.44.1.1.6.1.1.3.1 i 5");
	    LOGGER.info("STEP 1: EXPECTED: Should  be able to add the IPV4 ping server table 1");
	    LOGGER.info("#####################################################################################");
	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_ONE_ADD.getOid(), SnmpDataType.INTEGER,
		    BroadBandTestConstants.CONSTANT_PING_SERVER_ADDTABLE,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_ONE_ADD.getTableIndex());
	    errorMessage = "Unable to Add the IPV4 ping server table1 using SNMP";
	    LOGGER.info("STEP 1: ACTUAL: "
		    + (status ? "Successfully Added the IPV4 ping server table1 using SNMP" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 2:Verify setting the IPV4 ping server 1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.2.1) as
	     * '1.2.3.4'.
	     * 
	     */

	    testStepNumber = "s2";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 2: DESCRIPTION: Verify setting  the IPV4 ping server 1  using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.2.1) as '1.2.3.4'");
	    LOGGER.info(
		    "STEP 2: ACTION: Execute the below snmp command to set the  IPV4 ping server 1 as \"1.2.3.4\" , snmpset -v 2c -c string1 10.254.35.35 .1.3.6.1.4.1.17270.44.1.1.6.1.1.2.1 s 1.2.3.4");
	    LOGGER.info("STEP 2: EXPECTED: Should be able to set the IPV4 ping server 1 as '1.2.3.4'");
	    LOGGER.info("#####################################################################################");

	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_ONE.getOid(), SnmpDataType.STRING,
		    BroadBandTestConstants.FIRST_IPV4_PINGSERVER_URI,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_ONE.getTableIndex());
	    errorMessage = "Unable to set the IPV4 ping server table1 using SNMP as '1.2.3.4' ";
	    LOGGER.info("STEP 2: ACTUAL : "
		    + (status ? "Successfully set the IPV4 ping server table1 using SNMP as '1.2.3.4' "
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 3:Verify adding the IPV4 ping server table 2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.3.2).
	     * 
	     */
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    testStepNumber = "s3";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 3: DESCRIPTION: Verify adding the IPV4 ping server table 2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.3.2).");
	    LOGGER.info(
		    "STEP 3: ACTION: Execute the below snmp command to add the  IPV4  ping server 2 table,snmpset -v 2c -c string1 10.254.35.35 .1.3.6.1.4.1.17270.44.1.1.6.1.1.3.2 i 5");
	    LOGGER.info("STEP 3: EXPECTED: Should  be able to add the IPV4 ping server table 2");
	    LOGGER.info("#####################################################################################");
	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_TWO_ADD.getOid(), SnmpDataType.INTEGER,
		    BroadBandTestConstants.CONSTANT_PING_SERVER_ADDTABLE,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_TWO_ADD.getTableIndex());
	    errorMessage = "Unable to Add the IPV4 ping server table2 using SNMP";
	    LOGGER.info("STEP 3: ACTUAL : "
		    + (status ? "Successfully Added the IPV4 ping server table2 using SNMP" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 4:Verify setting the IPV4 ping server 2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.2.2) as
	     * '2.3.4.5'.
	     * 
	     */
	    testStepNumber = "s4";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 4: DESCRIPTION: Verify setting  the IPV4 ping server 2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.2.2) as '2.3.4.5'");
	    LOGGER.info(
		    "STEP 4: ACTION: Execute the below snmp command to set the  IPV4 ping server 2 as \"2.3.4.5\"snmpset -v 2c -c string1 10.254.35.35 .1.3.6.1.4.1.17270.44.1.1.6.1.1.2.2 s 2.3.4.5");
	    LOGGER.info("STEP 4: EXPECTED: Should be able to set the IPV4 ping server 2 as '2.3.4.5'");
	    LOGGER.info("#####################################################################################");

	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_TWO.getOid(), SnmpDataType.STRING,
		    BroadBandTestConstants.SECOND_IPV4_PINGSERVER_URI,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_TWO.getTableIndex());

	    errorMessage = "Unable to set the IPV4 ping server table2 using SNMP as '2.3.4.5' ";
	    LOGGER.info("STEP 4: ACTUAL : "
		    + (status ? "Successfully set the IPV4 ping server table2 using SNMP as '2.3.4.5' "
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 5:Verify adding the IPV4 ping server table 3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.3.3).
	     * 
	     */
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    testStepNumber = "s5";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 5: DESCRIPTION: Verify adding the IPV4 ping server table 3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.3.3).");
	    LOGGER.info(
		    "STEP 5: ACTION: Execute the below snmp command to add the IPV4 ping server 3 table, snmpset -v 2c -c string1 10.254.35.35 .1.3.6.1.4.1.17270.44.1.1.6.1.1.3.3 i 5");
	    LOGGER.info("STEP 5: EXPECTED: Should  be able to add the IPV4 ping server table 3");
	    LOGGER.info("#####################################################################################");
	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_THREE_ADD.getOid(), SnmpDataType.INTEGER,
		    BroadBandTestConstants.CONSTANT_PING_SERVER_ADDTABLE,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_THREE_ADD.getTableIndex());
	    errorMessage = "Unable to Add the IPV4 ping server table3 using SNMP";
	    LOGGER.info("STEP 5: ACTUAL : "
		    + (status ? "Successfully Added the IPV4 ping server table3 using SNMP" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 6:Verify setting the IPV4 ping server3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.2.1) to valid
	     * ipv4 ping server.
	     * 
	     */
	    testStepNumber = "s6";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 6: DESCRIPTION: Verify setting  the IPV4 ping server table 3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.2.1)");
	    LOGGER.info(
		    "STEP 6: ACTION: Execute the below snmp command to set the  IPV4 ping server 3 as \"<valid ipv4>\" snmpset -v 2c -c string1 10.254.35.35 .1.3.6.1.4.1.17270.44.1.1.6.1.1.2.3 s <valid ipv4>");
	    LOGGER.info("STEP 6: EXPECTED: Should be able to set the IPV4 ping server table 3 ");
	    LOGGER.info("#####################################################################################");
	    ipv4PingServer = BroadBandNetworkConnectivityUtils.resolvePingServer(device, tapEnv,
		    BroadBandTestConstants.IP_VERSION4);
	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_THREE.getOid(), SnmpDataType.STRING, ipv4PingServer,
		    BroadBandSnmpMib.ECM_IPV4_PING_SERVER_TABLE_THREE.getTableIndex());

	    errorMessage = "unable to set the IPV4 ping server in table3 using SNMP command";
	    LOGGER.info("STEP 6: ACTUAL : "
		    + (status ? "Successfully set the IPV4 ping server in table3 using SNMP" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 7:Verify adding the IPV6 ping server table 1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.3.1).
	     * 
	     */
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    testStepNumber = "s7";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 7: DESCRIPTION: Verify adding the IPV6 ping server table 1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.3.1).");
	    LOGGER.info(
		    "STEP 7: ACTION: Execute the below snmp command to add the  IPV6 ping server 1 table, snmpset -v 2c -c string1 10.254.35.35 .1.3.6.1.4.1.17270.44.1.1.6.2.1.3.1 i 5");
	    LOGGER.info("STEP 7: EXPECTED: Should be able to add the IPV6 ping server table 1");
	    LOGGER.info("#####################################################################################");

	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_ONE_ADD.getOid(), SnmpDataType.INTEGER,
		    BroadBandTestConstants.CONSTANT_PING_SERVER_ADDTABLE,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_ONE_ADD.getTableIndex());
	    errorMessage = "Unable to add the the IPV6 ping server table1 using SNMP";
	    LOGGER.info("STEP 7: ACTUAL : "
		    + (status ? "Successfully Added the IPV6 ping server table1 using SNMP" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 8:Verify setting the IPV6 ping server1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.2.1) as
	     * '2001::2002'.
	     * 
	     */
	    testStepNumber = "s8";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 8: DESCRIPTION: Verify setting  the IPV6 ping server 1 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.2.1) as '2001::2002'.");
	    LOGGER.info(
		    "STEP 8: ACTION: Execute the below snmp command to set the  IPV4 ping server 1 as \"2001::2002\" snmpset -v 2c -c string1 10.254.35.35 .1.3.6.1.4.1.17270.44.1.1.6.2.1.2.1 s 2001::2002");
	    LOGGER.info("STEP 8: EXPECTED: Should be able to set the IPV6 ping server 1 as '2001::2002'");
	    LOGGER.info("#####################################################################################");
	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_ONE.getOid(), SnmpDataType.STRING,
		    BroadBandTestConstants.FIRST_IPV6_PINGSERVER_URI,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_ONE.getTableIndex());

	    errorMessage = "Unable to set the IPV6 ping server table1 using SNMP as '2001::2002' ";
	    LOGGER.info("STEP 8: ACTUAL : "
		    + (status ? "Successfully set the IPV6 ping server table1 using SNMP as '2001::2002' "
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 9:Verify adding the IPV6 ping server table 2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.3.2).
	     * 
	     */
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    testStepNumber = "s9";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 9: DESCRIPTION: Verify adding the IPV6 ping server table 2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.3.2).");
	    LOGGER.info(
		    "STEP 9: ACTION: Execute the below snmp command to add the  IPV4 ping server 2 table,snmpset -v 2c -c string1 10.254.35.35 .1.3.6.1.4.1.17270.44.1.1.6.2.1.3.2 i 5");
	    LOGGER.info("STEP 9: EXPECTED: Should  be able to add the IPV6 ping server table 2");
	    LOGGER.info("#####################################################################################");

	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_TWO_ADD.getOid(), SnmpDataType.INTEGER,
		    BroadBandTestConstants.CONSTANT_PING_SERVER_ADDTABLE,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_TWO_ADD.getTableIndex());
	    errorMessage = "Unable to add the the IPV6 ping server table2 using SNMP";
	    LOGGER.info("STEP 9: ACTUAL : "
		    + (status ? "Successfully Added the IPV6 ping server table2 using SNMP" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 10:Verify setting the IPV6 ping server2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.2.2) as
	     * '2002::2003'
	     * 
	     */
	    testStepNumber = "s10";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 10: DESCRIPTION: Verify setting  the IPV6 ping server 2 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.2.2) as '2002::2003'");
	    LOGGER.info(
		    "STEP 10: ACTION: Execute the below snmp command to set the  IPV4 ping server 2 as \"2002::2003\"snmpset -v 2c -c string1 10.254.35.35 .1.3.6.1.4.1.17270.44.1.1.6.2.1.2.2 s 2002::2003");
	    LOGGER.info("STEP 10: EXPECTED: Should be able to set the IPV6 ping server 2 as '2002::2003'");
	    LOGGER.info("#####################################################################################");

	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_TWO.getOid(), SnmpDataType.STRING,
		    BroadBandTestConstants.SECOND_IPV6_PINGSERVER_URI,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_TWO.getTableIndex());

	    errorMessage = "Unable to set the IPV6 ping server table2 using SNMP as '2002::2003' ";
	    LOGGER.info("STEP 10: ACTUAL : "
		    + (status ? "Successfully set the IPV6 ping server table2 using SNMP as '2002::2003' "
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 11:Verify adding the IPV6 ping server table 3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.3.3).
	     * 
	     */
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    testStepNumber = "s11";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 11: DESCRIPTION: Verify adding the IPV6 ping server table 3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.2.1.3.3).");
	    LOGGER.info(
		    "STEP 11: ACTION: Execute the below snmp command to add the  IPV4 ping server 3 table, snmpset -v 2c -c string1 10.254.35.35 .1.3.6.1.4.1.17270.44.1.1.6.2.1.3.3 i 5");
	    LOGGER.info("STEP 11: EXPECTED: Should  be able to add the IPV6 ping server table 3");
	    LOGGER.info("#####################################################################################");

	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_THREE_ADD.getOid(), SnmpDataType.INTEGER,
		    BroadBandTestConstants.CONSTANT_PING_SERVER_ADDTABLE,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_THREE_ADD.getTableIndex());
	    errorMessage = "Unable to Add the IPV6 ping server table3 using SNMP";
	    LOGGER.info("STEP 11: ACTUAL : "
		    + (status ? "Successfully Added the IPV6 ping server table3 using SNMP" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 12:Verify setting the IPV4 ping server3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.2.1) to
	     * valid ipv4 ping server.
	     * 
	     */
	    testStepNumber = "s12";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 12: DESCRIPTION: Verify setting  the IPV6 ping server table 3 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.6.1.1.2.1)");
	    LOGGER.info(
		    "STEP 12: ACTION: Execute the below snmp command to set the  IPV4 ping server 3 as \"<valid ipv6>\" snmpset -v 2c -c string1 10.254.35.35 .1.3.6.1.4.1.17270.44.1.1.6.2.1.2.3 s <valid ipv6>");
	    LOGGER.info("STEP 12: EXPECTED: Should be able to set the IPV6 ping server table 3 ");
	    LOGGER.info("#####################################################################################");
	    ipv6PingServer = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_WAN_IPV6);
	    status = BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_THREE.getOid(), SnmpDataType.STRING, ipv6PingServer,
		    BroadBandSnmpMib.ECM_IPV6_PING_SERVER_TABLE_THREE.getTableIndex());
	    errorMessage = "unable to set the IPV6 ping server in table3 using SNMP command";
	    LOGGER.info("STEP 12: ACTUAL : "
		    + (status ? "Successfully set the IPV6 ping server in table3 using SNMP" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 13 :verify whether "PING_FAILED:1.2.3.4" message is present in self heallog.txt.0 after the setting
	     * the InValid servers.
	     */
	    testStepNumber = "s13";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 13: DESCRIPTION: Verify whether 'PING_FAILED:1.2.3.4' message  is present in self heallog.txt.0  after the setting the InValid  servers.");
	    LOGGER.info(
		    "STEP 13: ACTION: validate whether \"PING_FAILED:1.2.3.4\" message  is present in self heallog.txt.0  after the setting the required false servers. Command is EXAMPLE \"grep -i \"PING_FAILED:1.2.3.4\" /rdklogs/logs/SelfHeal.txt.0\".");
	    LOGGER.info(
		    "STEP 13: EXPECTED:  'PING_FAILED:1.2.3.4' failure message  should  get logged in the next self heal window");
	    LOGGER.info("#####################################################################################");
	    try {
		status = BroadBandCommonUtils
			.searchLogFiles(tapEnv, device, BroadBandTraceConstants.PING_SERVER_IPV4_FIRST_URI_FAIL,
				BroadBandCommandConstants.FILE_SELFHEAL_LOG,
				BroadBandTestConstants.THIRTY_MINUTES_IN_MILLIS
					+ BroadBandTestConstants.TWENTY_MINUTES_IN_MILLIS,
				BroadBandTestConstants.ONE_MINUTE_IN_MILLIS)
			.contains(BroadBandTraceConstants.PING_SERVER_IPV4_FIRST_URI_FAIL.replace("\"", ""));
	    } catch (TestException exception) {
		errorMessage = exception.getMessage();
		LOGGER.error("error occured while checking the 'PING_FAILED:1.2.3.4' message in self heallog.txt.0  "
			+ errorMessage);
	    }
	    errorMessage = "'PING_FAILED:1.2.3.4' failure message  is not logged in the next self heal window";
	    LOGGER.info("STEP 13: ACTUAL : "
		    + (status ? " 'PING_FAILED:1.2.3.4' failure message is logged in the next self heal window"
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 14:verify whether "PING_FAILED:2.3.4.5" message is present in self heallog.txt.0 after the setting
	     * the InValid servers.
	     */
	    testStepNumber = "s14";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 14: DESCRIPTION:  verify whether 'PING_FAILED:2.3.4.5' message  is present in self heallog.txt.0  after the setting the InValid  servers.");
	    LOGGER.info(
		    "STEP 14: ACTION: validate whether \"PING_FAILED:2.3.4.5\" message  is present in self heallog.txt.0  after the setting the required false servers. Command is  \"grep -i \"PING_FAILED:2.3.4.5\" /rdklogs/logs/SelfHeal.txt.0\".\r\n"
			    + "");
	    LOGGER.info(
		    "STEP 14: EXPECTED:  'PING_FAILED:2.3.4.5' failure message  should  get logged in the next self heal window");
	    LOGGER.info("#####################################################################################");
	    try {
		status = BroadBandCommonUtils
			.searchLogFiles(tapEnv, device, BroadBandTraceConstants.PING_SERVER_IPV4_SECOND_URI_FAIL,
				BroadBandCommandConstants.FILE_SELFHEAL_LOG,
				BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)
			.contains(BroadBandTraceConstants.PING_SERVER_IPV4_SECOND_URI_FAIL.replace("\"", ""));
	    } catch (TestException exception) {
		errorMessage = exception.getMessage();
		LOGGER.error("error occured while checking the 'PING_FAILED:2.3.4.5' message in self heallog.txt.0  "
			+ errorMessage);
	    }
	    errorMessage = "'PING_FAILED:2.3.4.5' failure message  is not logged in the next self heal window";
	    LOGGER.info("STEP 14: ACTUAL : "
		    + (status ? " 'PING_FAILED:2.3.4.5' failure message is logged in the next self heal window"
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 15 :verify whether "PING_FAILED:2001::2002" message is present in self heallog.txt.0 after the
	     * setting the InValid servers.
	     */
	    testStepNumber = "s15";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 15: DESCRIPTION:  verify whether 'PING_FAILED:2001::2002' message  is present in self heallog.txt.0  after the setting the InValid  servers.");
	    LOGGER.info(
		    "STEP 15: ACTION: validate whether \"PING_FAILED:2001:2002\" message  is present in self heallog.txt.0  after the setting  required false servers.Command is  \"grep -i \"PING_FAILED:2001:2002\" /rdklogs/logs/SelfHeal.txt.0\".");
	    LOGGER.info(
		    "STEP 15: EXPECTED:  'PING_FAILED:2001::2002' failure message  should  get logged in the next self heal window");
	    LOGGER.info("#####################################################################################");
	    try {
		status = BroadBandCommonUtils
			.searchLogFiles(tapEnv, device, BroadBandTraceConstants.PING_SERVER_IPV6_FIRST_URI_FAIL,
				BroadBandCommandConstants.FILE_SELFHEAL_LOG,
				BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
				BroadBandTestConstants.ONE_MINUTE_IN_MILLIS)
			.contains(BroadBandTraceConstants.PING_SERVER_IPV6_FIRST_URI_FAIL.replace("\"", ""));

	    } catch (TestException exception) {
		errorMessage = exception.getMessage();
		LOGGER.error("error occured while checking the 'PING_FAILED:2001::2002' message in self heallog.txt.0  "
			+ errorMessage);
	    }
	    errorMessage = "'PING_FAILED:2001::2002' failure message  is not logged in the next self heal window";
	    LOGGER.info("STEP 15: ACTUAL : "
		    + (status ? " 'PING_FAILED:2001:2002' failure message is logged in the next self heal window"
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 16 :verify whether "PING_FAILED:2002::2003" message is present in self heallog.txt.0 after the
	     * setting the InValid servers.
	     */
	    testStepNumber = "s16";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 16: DESCRIPTION:  verify whether 'PING_FAILED:2002::2003' message  is present in self heallog.txt.0  after the setting the InValid  servers.");
	    LOGGER.info(
		    "STEP 16: ACTION: validate whether \"PING_FAILED:2002:2003\" message  is present in self heallog.txt.0  after the setting the required false servers. Command is  \"grep -i \"PING_FAILED:2002:2003\" /rdklogs/logs/SelfHeal.txt.0\".");
	    LOGGER.info(
		    "STEP 16: EXPECTED:  'PING_FAILED:2002::2003' failure message  should  get logged in the next self heal window");
	    LOGGER.info("#####################################################################################");
	    try {
		status = BroadBandCommonUtils
			.searchLogFiles(tapEnv, device, BroadBandTraceConstants.PING_SERVER_IPV6_SECOND_URI_FAIL,
				BroadBandCommandConstants.FILE_SELFHEAL_LOG,
				BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)
			.contains(BroadBandTraceConstants.PING_SERVER_IPV6_SECOND_URI_FAIL.replace("\"", ""));

	    } catch (TestException exception) {
		errorMessage = exception.getMessage();
		LOGGER.error("error occured while checking the 'PING_FAILED:2002::2003' message in self heallog.txt.0  "
			+ errorMessage);
	    }
	    errorMessage = "'PING_FAILED:2002:2003' failure message  is not logged in the next self heal window";
	    LOGGER.info("STEP 16: ACTUAL : "
		    + (status ? " 'PING_FAILED:2002:2003' failure message is logged in the next self heal window"
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 17 :verify whether "Connectivity successfull" message is present in rdklogs/logs/selfheallog.txt.0
	     * after the setting the Invalid servers.
	     */
	    testStepNumber = "s17";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 17: DESCRIPTION: verify whether 'Connectivity successfull' message  is present in rdklogs/logs/selfheallog.txt.0 after the setting the Invalid  servers.");
	    LOGGER.info(
		    "STEP 17: ACTION: validate whether \"Connectivity Test is successfull\" message  is present in rdklogs/logs/selfheallog.txt.0 .");
	    LOGGER.info(
		    "STEP 17: EXPECTED:  Connectivity successfull Log  message  should  get logged in the next self heal window");
	    LOGGER.info("#####################################################################################");
	    try {
		status = BroadBandCommonUtils
			.searchLogFiles(tapEnv, device, BroadBandTraceConstants.PING_SERVER_SUCCESS_LOG_MESSAGE,
				BroadBandCommandConstants.FILE_SELFHEAL_LOG, BroadBandTestConstants.FIVE_MINUTES,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)
			.contains(BroadBandTraceConstants.PING_SERVER_SUCCESS_LOG_MESSAGE.replace("\"", ""));
	    } catch (TestException exception) {
		errorMessage = exception.getMessage();
		LOGGER.error(
			"error occured while checking the 'ping to the other list is successfull' message in self heallog.txt.0  "
				+ errorMessage);
	    }
	    errorMessage = "'Connectivity successfull' log message is not logged in the next self heal window";
	    LOGGER.info("STEP 17: ACTUAL : "
		    + (status ? "'Connectivity successfull' log message is successful in the next self heal window"
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	} catch (Exception testException) {
	    errorMessage = "Exception occured while checking the valid and invalid ping server connectivity test "
		    + testException.getMessage();
	    LOGGER.error(errorMessage);
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "POST-CONDITION 1: DESCRIPTION : Clear/Reset the  table row of ipv4 by adding a single table row");
	    LOGGER.info("POST-CONDITION : ACTION : Verify adding a table row as a ipv4 single server");
	    LOGGER.info("POST-CONDITION :EXPECTED: Should be able to add the add  table as a ipv4 single server");
	    LOGGER.info("#####################################################################################");
	    pingServersTable.clear();
	    pingServers.clear();
	    tableRowipv4.clear();
	    tableRowipv6.clear();
	    pingServers.add(BroadBandTestConstants.THIRD_IPV4_PINGSERVER_URI);
	    pingServersTable.put(BroadBandWebPaConstants.WEBPA_IPV4_PING_SERVER_URI, pingServers);
	    webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
		    BroadBandWebPaConstants.WEBPA_IPV4_PING_ADD_TABLE, pingServersTable);
	    tableRowNumber = webPaServerResponse.getRow();
	    tableRowipv4.add(tableRowNumber);
	    status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
	    if (status) {
		LOGGER.info("Added a single table row for Ipv4 ping server");
	    }

	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "POST-CONDITION 2: DESCRIPTION : Clear/Reset the  table row of ipv6 by adding a single table row");
	    LOGGER.info("POST-CONDITION : ACTION : Verify adding a table row as a ipv6 single server");
	    LOGGER.info("POST-CONDITION :EXPECTED: Should be able to add the add  table as a ipv6 single server");
	    LOGGER.info("#####################################################################################");
	    pingServersTable.clear();
	    pingServers.clear();
	    pingServers.add(BroadBandTestConstants.THIRD_IPV6_PINGSERVER_URI);
	    pingServersTable.put(BroadBandWebPaConstants.WEBPA_IPV6_PING_SERVER_URI, pingServers);
	    webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
		    BroadBandWebPaConstants.WEBPA_IPV6_PING_ADD_TABLE, pingServersTable);
	    tableRowNumber = webPaServerResponse.getRow();
	    tableRowipv6.add(tableRowNumber);
	    status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
	    if (status) {
		LOGGER.info("Added a single table row for Ipv6 ping server");
	    }

	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "POST-CONDITION 3: DESCRIPTION : delete the added ipv4 ping servers in the Ping Server Table by using WEBPA DELETE command.");
	    LOGGER.info(
		    "POST-CONDITION : ACTION : Delete the added Ping Servers in the ipv4 Ping Server Table by using WEBPA DELETE command");
	    LOGGER.info(
		    "POST-CONDITION :EXPECTED: Should be able to delete the added ipv4 ping servers in the Ping Server Table using webpa Delete command");
	    LOGGER.info("#####################################################################################");
	    String num = CommonMethods.patternFinder(tableRowipv4.get(0),
		    BroadBandTestConstants.PATTERN_MATCHER_TO_GET_TABLE_ROWS);
	    for (int count = 1; count <= Integer.parseInt(num); count++) {
		webPaServerResponse = tapEnv.deleteTableRowUsingRestApi(device,
			"Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable." + count + ".");
		status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
		if (status) {
		    LOGGER.info("Deleted ping servers table row="
			    + "Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable." + count + ".");
		}

	    }

	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "POST-CONDITION 4: DESCRIPTION : delete the added ipv6 ping servers in the Ping Server Table by using WEBPA DELETE command.");
	    LOGGER.info(
		    "POST-CONDITION : ACTION : Delete the added Ping Servers in the ipv6 Ping Server Table by using WEBPA DELETE command");
	    LOGGER.info(
		    "POST-CONDITION :EXPECTED: Should be able to delete the added ipv6 ping servers in the Ping Server Table using webpa Delete command");
	    LOGGER.info("#####################################################################################");
	    num = CommonMethods.patternFinder(tableRowipv6.get(0),
		    BroadBandTestConstants.PATTERN_MATCHER_TO_GET_TABLE_ROWS);
	    for (int count = 1; count <= Integer.parseInt(num); count++) {
		webPaServerResponse = tapEnv.deleteTableRowUsingRestApi(device,
			"Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable." + count + ".");
		status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
		if (status) {
		    LOGGER.info("Deleted ping servers table row for ipv6="
			    + "Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable." + count + ".");
		}

	    }

	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");

	}
    }

    /**
     * Test case is created as part of COVERAGE AUTOMATION- Ping Server to detect network connectivity
     * 
     * 
     * TC-RDKB-NW-CONNECTIVITY-1002: Verify the ping related default values in the device after factory resetting by SNMP.
     *
     * <ol>
     * <li>S1) Verify retrieving the value of Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_NumPingsPerServer using
     * SNMP OID (.1.3.6.1.4.1.17270.44.1.1.2.0)</li>
     * <li>S2) Verify retrieving the value of Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_MinNumPingServer to get
     * the minNumPingServer using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.3.0)</li>
     * <li>S3) Verify retrieving the value ofDevice.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval to get the
     * pingInterval using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.4.0)</li>
     * <li>S4) Verify retrieving the value ofDevice.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_PingRespWaitTime to get
     * the PingRespWaitTime using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.5.0)</li>
     * <li>S5) Verify retrieving the value ofDevice.SelfHeal.ResourceMonitor.X_RDKCENTRAL-COM_UsageComputeWindow to get
     * the resourceUsageComputeWindow using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.7.0)</li>
     * <li>S6) Verify retrieving the value ofDevice.SelfHeal.ResourceMonitor.X_RDKCENTRAL-COM_AvgCPUThreshold to get the
     * AvgCPUThreshold using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.8.0)</li>
     * <li>S7) Verify retrieving the value ofDevice.SelfHeal.ResourceMonitor.X_RDKCENTRAL-COM_AvgMemoryThreshold to get
     * the avgMemoryThreshold using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.9.0)</li>
     * <li>S8) Verify retrieving the value ofDevice.SelfHeal.X_RDKCENTRAL-COM_MaxRebootCount to get the MaxRebootCount
     * using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.10.0)</li>
     * <li>S9) Verify retrieving the value ofDevice.SelfHeal.X_RDKCENTRAL-COM_MaxResetCount to get the MaxResetCount
     * using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.11.0)</li>
     * <li>S10) Verify setting the value ofDevice.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval to Set the
     * pingInterval as 14 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.4.0)</li>
     * <li>S11) Verify setting the value ofDevice.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval to Set the
     * pingInterval as 1441 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.4.0)</li>
     * <li>S12) Verify setting the value ofDevice.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval to Set the
     * pingInterval between 14 and 1440 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.4.0)</li>
     * </ol>
     *
     * @author Joseph Maduram, INFOSYS
     * @refactor anandam
     * 
     * @param device
     *            {@link Dut}
     */

    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    BroadBandTestGroup.WEBPA }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-NW-CONNECTIVITY-1002", testDecription = "Verify whether gateway ipv4 and Gateway ipv6 ping is successful")

    public void connectivityTestDefaultSettingBySnmp(Dut device) {
	// String to store the test case ID
	String testId = "TC-RDKB-NW-CONNECTIVITY-102";
	// String to store the test step number
	String testStepNumber = "s1";
	// String to store the error message
	String errorMessage = null;
	// stores the test status
	boolean status = false;
	// Stores the factory reset status
	boolean isFactoryReset = false;
	// stores the response
	String response = null;
	// integer to store pre condition step number
	int preConStepNumber = 1;
	// stores default maxResetCount value
	String defaultMaxResetCount = BroadBandTestConstants.STRING_VALUE_THREE;
	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    isFactoryReset = BroadBandPreConditionUtils.executePreConditionToFactoryResetDevice(device, tapEnv,
		    preConStepNumber);
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-NW-CONNECTIVITY-1002");
	    LOGGER.info(
		    "TEST DESCRIPTION: Verify the ping related default values in the device after factory resetting by SNMP.");

	    /**
	     * STEP 1:Verify retrieving the value of Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_NumPingsPerServer
	     * using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.2.0)
	     */
	    tapEnv.waitTill(BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS);
	    testStepNumber = "s1";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 1:Verify retrieving the value of Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_NumPingsPerServer using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.2.0)");
	    LOGGER.info("EXPECTED-Should be able to get the default value of NumPingperServer using Snmp Command");
	    LOGGER.info("#####################################################################################");
	    response = BroadBandSnmpUtils.executeSnmpGetOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.ECM_SELFHEAL_NO_OF_PINGS_PER_SERVER.getOid() + ".0");
	    LOGGER.info("NumPingPerServer retrieved using SNMP = " + response);
	    status = CommonMethods.isNotNull(response)
		    && response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_THREE);
	    errorMessage = "Unable to verify the NumPingsPerServer using SNMP  -Expected value:"
		    + BroadBandTestConstants.STRING_VALUE_THREE + "|Actual Value:" + response;
	    LOGGER.info("S1 ACTUAL : " + (status
		    ? "Successfully verified  the default NumPingperServer using SNMP command.-Expected value:"
			    + BroadBandTestConstants.STRING_VALUE_THREE + "|Actual Value:" + response
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 2:Verify retrieving the value of Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_MinNumPingServer
	     * to get the minNumPingServer using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.3.0)
	     */
	    testStepNumber = "s2";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 2:Verify retrieving the value of Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_MinNumPingServer to get the minNumPingServer using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.3.0)");
	    LOGGER.info("EXPECTED-Should be able to get the default value of MinNumPingServer as 1 using SNMP Command");
	    LOGGER.info("#####################################################################################");
	    response = BroadBandSnmpUtils.executeSnmpGetOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.ECM_SELFHEAL_MIN_NO_OF_PING_SERVER.getOid() + ".0");
	    LOGGER.info("minNumPingServer retrieved using SNMP = " + response);
	    status = CommonMethods.isNotNull(response)
		    && response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_ONE);
	    errorMessage = "Unable to verify the MinNumPingServer  using SNMP command-Expected value:"
		    + BroadBandTestConstants.STRING_VALUE_ONE + "|Actual Value:" + response;
	    LOGGER.info("S2 ACTUAL : " + (status
		    ? "Successfully verified  the default MinNumPingServer using SNMP command.-Expected value:"
			    + BroadBandTestConstants.STRING_VALUE_ONE + "|Actual Value:" + response
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 3:Verify retrieving the value ofDevice.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval to
	     * get the pingInterval using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.4.0)
	     */
	    testStepNumber = "s3";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 3:Verify retrieving the value ofDevice.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval  to get the pingInterval using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.4.0)");
	    LOGGER.info("EXPECTED-Should be able to get the default value of pingInterval as 60 using SNMP Command");
	    LOGGER.info("#####################################################################################");
	    response = BroadBandSnmpUtils.executeSnmpGetOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.ECM_SELFHEAL_PING_INTERVAL.getOid() + ".0");
	    LOGGER.info("pingInterval retrieved using SNMP = " + response);
	    status = CommonMethods.isNotNull(response)
		    && response.equalsIgnoreCase(BroadBandTestConstants.CONSTANT_DEFAULT_PING_INTERVAL);
	    errorMessage = "Unable to verify the pingInterval  using SNMP command.";
	    LOGGER.info("S3 ACTUAL : " + (status
		    ? "Successfully verified  the default pingInterval using SNMP command-Expected value:"
			    + BroadBandTestConstants.CONSTANT_DEFAULT_PING_INTERVAL + "|Actual Value:" + response
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 4:Verify retrieving the value ofDevice.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_PingRespWaitTime
	     * to get the PingRespWaitTime using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.5.0)
	     */
	    testStepNumber = "s4";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 4:Verify retrieving the value ofDevice.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_PingRespWaitTime  to get the PingRespWaitTime using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.5.0)");
	    LOGGER.info("EXPECTED- Should be able to get the PingRespWaitTime  default value as 1000");
	    LOGGER.info("#####################################################################################");
	    response = BroadBandSnmpUtils.executeSnmpGetOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.ECM_SELFHEAL_PING_RESPONSE_WAIT_TIME.getOid() + ".0");
	    LOGGER.info("pingRespWaitTime retrieved using SNMP = " + response);
	    status = CommonMethods.isNotNull(response)
		    && response.equalsIgnoreCase(BroadBandTestConstants.CONSTANT_PING_RESP_WAIT_TIME);
	    errorMessage = "Unable to verify the pingRespWaitTime  using SNMP command.";
	    LOGGER.info("S4 ACTUAL : " + (status
		    ? "Successfully verified  the default PingRespWaitTime using SNMP command.-Expected value:"
			    + BroadBandTestConstants.CONSTANT_PING_RESP_WAIT_TIME + "|Actual Value:" + response
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 5:Verify retrieving the value of Device.SelfHeal.ResourceMonitor.X_RDKCENTRAL-COM_UsageComputeWindow
	     * to get the resourceUsageComputeWindow using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.7.0)
	     */
	    testStepNumber = "s5";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 5:Verify retrieving the value of Device.SelfHeal.ResourceMonitor.X_RDKCENTRAL-COM_UsageComputeWindow  to get the resourceUsageComputeWindow using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.7.0)");
	    LOGGER.info("EXPECTED- Should be able to get the UsageComputeWindow  default value as 15");
	    LOGGER.info("#####################################################################################");
	    response = BroadBandSnmpUtils.executeSnmpGetOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.ECM_SELFHEAL_RESOURCE_USAGE_COMPUTER_WINDOW.getOid() + ".0");
	    LOGGER.info("UsageComputeWindow retrieved using SNMP = " + response);
	    status = CommonMethods.isNotNull(response)
		    && response.equalsIgnoreCase(BroadBandTestConstants.CONSTANT_RESOURCE_USAGE_COMPUTE_WINDOW);
	    errorMessage = "Unable to verify the UsageComputeWindow  using SNMP command.";
	    LOGGER.info("S5 ACTUAL : " + (status
		    ? "Successfully verified  the default UsageComputeWindow  using SNMP command.-Expected value:"
			    + BroadBandTestConstants.CONSTANT_RESOURCE_USAGE_COMPUTE_WINDOW + "|Actual Value:"
			    + response
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 6:Verify retrieving the value ofDevice.SelfHeal.ResourceMonitor.X_RDKCENTRAL-COM_AvgCPUThreshold to
	     * get the AvgCPUThreshold using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.8.0)
	     */
	    testStepNumber = "s6";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 6:Verify retrieving the value ofDevice.SelfHeal.ResourceMonitor.X_RDKCENTRAL-COM_AvgCPUThreshold to get the AvgCPUThreshold using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.8.0)");
	    LOGGER.info("EXPECTED- Should be able to get the AvgCPUThreshold default value as 100");
	    LOGGER.info("#####################################################################################");
	    response = BroadBandSnmpUtils.executeSnmpGetOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.ECM_SELFHEAL_AVG_CPU_THRESHOLD.getOid() + ".0");
	    LOGGER.info("AvgCPUThreshold retrieved using SNMP = " + response);
	    status = CommonMethods.isNotNull(response)
		    && response.equalsIgnoreCase(BroadBandTestConstants.CONSTANT_AVG_CPU_THRESHOLD);
	    errorMessage = "Unable to verify the avgCPUThreshold  using SNMP command.";
	    LOGGER.info("S6 ACTUAL : " + (status
		    ? "Successfully verified  the default AvgCPUThreshold  using SNMP command.-Expected value:"
			    + BroadBandTestConstants.CONSTANT_AVG_CPU_THRESHOLD + "|Actual Value:" + response
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 7:Verify retrieving the value ofDevice.SelfHeal.ResourceMonitor.X_RDKCENTRAL-COM_AvgMemoryThreshold
	     * to get the avgMemoryThreshold using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.9.0)
	     */
	    testStepNumber = "s7";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 7:Verify retrieving the value ofDevice.SelfHeal.ResourceMonitor.X_RDKCENTRAL-COM_AvgMemoryThreshold to get the avgMemoryThreshold using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.9.0)  ");
	    LOGGER.info("EXPECTED- Should be able to get the AvgMemoryThreshold  default value as 100");
	    LOGGER.info("#####################################################################################");
	    response = BroadBandSnmpUtils.executeSnmpGetOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.ECM_SELFHEAL_AVG_MEMORY_THRESHOLD.getOid() + ".0");
	    LOGGER.info("AvgMemoryThreshold retrieved using SNMP = " + response);
	    status = CommonMethods.isNotNull(response)
		    && response.equalsIgnoreCase(BroadBandTestConstants.CONSTANT_AVG_MEMORY_THRESHOLD);
	    errorMessage = "Unable to verify the AvgMemoryThreshold  using SNMP command.";
	    LOGGER.info("S7 ACTUAL : " + (status
		    ? "Successfully verified the default AvgMemoryThreshold   using SNMP command.-Expected value:"
			    + BroadBandTestConstants.CONSTANT_AVG_MEMORY_THRESHOLD + "|Actual Value:" + response
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 8:Verify retrieving the value ofDevice.SelfHeal.X_RDKCENTRAL-COM_MaxRebootCount to get the
	     * MaxRebootCount using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.10.0)
	     */
	    testStepNumber = "s8";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 8:Verify retrieving the value ofDevice.SelfHeal.X_RDKCENTRAL-COM_MaxRebootCount to get the MaxRebootCount using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.10.0)");
	    LOGGER.info("EXPECTED- Should be able to get the MaxRebootCount  default value as 3");
	    LOGGER.info("#####################################################################################");
	    response = BroadBandSnmpUtils.executeSnmpGetOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.ECM_SELFHEAL_MAX_REBOOT_COUNT.getOid() + ".0");
	    LOGGER.info("MaxRebootCount retrieved using SNMP = " + response);
	    status = CommonMethods.isNotNull(response)
		    && response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_THREE);
	    errorMessage = "Unable to verify the MaxRebootCount using SNMP command.";
	    LOGGER.info(
		    "S8 ACTUAL : " + (status
			    ? "Successfully verified  the default MaxRebootCount  using SNMP command.-Expected value:"
				    + BroadBandTestConstants.STRING_VALUE_THREE + "|Actual Value:" + response
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 9:Verify retrieving the value ofDevice.SelfHeal.X_RDKCENTRAL-COM_MaxResetCount to get the
	     * MaxResetCount using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.11.0)
	     */
	    testStepNumber = "s9";
	    status = false;

	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 9:Verify retrieving the value ofDevice.SelfHeal.X_RDKCENTRAL-COM_MaxResetCount to get the  MaxResetCount using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.11.0)");
	    LOGGER.info(
		    "EXPECTED- Should be able to get the MaxResetCount default value as  3 for master and stable builds and release builld");
	    LOGGER.info("#####################################################################################");
	    response = BroadBandSnmpUtils.executeSnmpGetOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.ECM_SELFHEAL_MAX_SUB_SYSTEM_RESET_COUNT.getOid() + ".0");
	    defaultMaxResetCount = BroadbandPropertyFileHandler.getDefaultMaxResetCount();
	    LOGGER.info("MaxResetCount retrieved using SNMP = " + response);
	    status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(defaultMaxResetCount);

	    errorMessage = "Unable to verify the MaxResetCount using SNMP command.";
	    LOGGER.info(
		    "S9 ACTUAL : " + (status
			    ? "Successfully verified the default MaxResetCount  using SNMP command.-Expected value:"
				    + defaultMaxResetCount + "|Actual Value:" + response
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 10:Verify retrieving the value ofDevice.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval to
	     * Set the pingInterval as 14 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.4.0)
	     */
	    testStepNumber = "s10";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 10:Verify retrieving the value ofDevice.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval  to Set the pingInterval  as 14 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.4.0)");
	    LOGGER.info("EXPECTED-Should not be able to set the pinginterval as 14");
	    LOGGER.info("#####################################################################################");
	    String snmpSetResponse = BroadBandSnmpUtils.retrieveSnmpSetOutputWithDefaultIndexOnRdkDevices(device,
		    tapEnv, BroadBandSnmpMib.ECM_SELFHEAL_PING_INTERVAL.getOid() + ".0", SnmpDataType.UNSIGNED_INTEGER,
		    BroadBandTestConstants.CONSTANT_PING_INTERVAL_14);
	    errorMessage = "Able to set  the Invalid ping interval using SNMP command.";
	    try {
		status = !(CommonMethods.isNotNull(snmpSetResponse)
			&& snmpSetResponse.equalsIgnoreCase(BroadBandTestConstants.CONSTANT_PING_INTERVAL_14));
	    } catch (Exception exception) {
		errorMessage = errorMessage + " " + exception.getMessage();
		LOGGER.error("Exception occurred during execution : ", exception);
	    }

	    LOGGER.info(
		    "S10 ACTUAL : " + (status ? " Not able to set the Invalid Ping Interval as 14  using SNMP command."
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 11:Verify retrieving the value ofDevice.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval to
	     * Set the pingInterval as 1441 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.4.0)
	     */
	    testStepNumber = "s11";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 11:Verify retrieving the value ofDevice.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval  to Set the pingInterval  as 1441 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.4.0)");
	    LOGGER.info("EXPECTED-Should not be able to set the pinginterval as 1441");
	    LOGGER.info("#####################################################################################");
	    snmpSetResponse = BroadBandSnmpUtils.retrieveSnmpSetOutputWithDefaultIndexOnRdkDevices(device, tapEnv,
		    BroadBandSnmpMib.ECM_SELFHEAL_PING_INTERVAL.getOid() + ".0", SnmpDataType.UNSIGNED_INTEGER,
		    BroadBandTestConstants.CONSTANT_PING_INTERVAL_1441);
	    errorMessage = "Able to set  the Invalid ping interval for  using SNMP command.";
	    try {
		status = !(CommonMethods.isNotNull(snmpSetResponse)
			&& snmpSetResponse.equalsIgnoreCase(BroadBandTestConstants.CONSTANT_PING_INTERVAL_1441));
	    } catch (Exception exception) {
		errorMessage = errorMessage + " " + exception.getMessage();
		LOGGER.error("Exception occurred during execution : ", exception);
	    }
	    LOGGER.info(
		    "S11 ACTUAL : " + (status ? "Not able to set the Invalid Ping Interval as 1441  using SNMP command."
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 12:Verify retrieving the value ofDevice.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval to
	     * Set the pingInterval as 1441 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.4.0)
	     */
	    testStepNumber = "s12";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 12:Verify retrieving the value ofDevice.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval  to Set the pingInterval  between 14 and  1440 using SNMP OID (.1.3.6.1.4.1.17270.44.1.1.4.0)");
	    LOGGER.info("EXPECTED-Should  be able to set the pinginterval between 14 and 1440");
	    LOGGER.info("#####################################################################################");
	    tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
	    snmpSetResponse = BroadBandSnmpUtils.retrieveSnmpSetOutputWithDefaultIndexOnRdkDevices(device, tapEnv,
		    BroadBandSnmpMib.ECM_SELFHEAL_PING_INTERVAL.getOid() + ".0", SnmpDataType.UNSIGNED_INTEGER,
		    BroadBandTestConstants.CONSTANT_DEFAULT_PING_INTERVAL);
	    status = CommonMethods.isNotNull(snmpSetResponse)
		    && snmpSetResponse.equalsIgnoreCase(BroadBandTestConstants.CONSTANT_DEFAULT_PING_INTERVAL);
	    errorMessage = "Unable to set  the valid ping interval between 14 and 1440 for using SNMP command.";
	    LOGGER.info("S12 ACTUAL : "
		    + (status ? " able to set the valid Ping Interval between 14 and 1440  using SNMP command."
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
	}

	catch (Exception testException) {
	    errorMessage = "Exception occured while checking the default Values: " + testException.getMessage();
	    LOGGER.error(errorMessage);
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    if (isFactoryReset) {
		LOGGER.info("POST-CONDITION : DESCRIPTION : BEGIN BROAD BAND DEVICE REACTIVATION.");
		LOGGER.info("POST-CONDITION : ACTION : BROAD BAND DEVICE REACTIVATION. ");
		LOGGER.info("POST-CONDITION : EXPECTED : device should get reactivated");
		LOGGER.info("### POST-CONDITION ### BEGIN BROAD BAND DEVICE REACTIVATION.");
		BroadBandWiFiUtils.reactivateDeviceUsingWebPa(tapEnv, device);
		LOGGER.info("### POST-CONDITION ### END BROAD BAND DEVICE REACTIVATION.");
		LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	    }
	}
    }
}